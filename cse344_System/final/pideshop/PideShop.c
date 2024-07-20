#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <time.h>
#include <signal.h>

#define MAX_ORDERS 100
#define MAX_SHOVELS 3
#define MAX_DELIVERY_ORDERS 3
#define OVEN_CAPACITY 6
#define BUFFER_SIZE 1024

typedef struct {
    int order_id;
    int customer_id;
    int x, y; // Customer location
    int status; // 0: placed, 1: prepared, 2: cooked, 3: in delivery, 4: delivered, -1: cancelled
} Order;

typedef struct {
    int id;
    int speed; // Delivery speed in m/min
    int current_orders[MAX_DELIVERY_ORDERS];
    int order_count;
    int deliveries_made;
    int total_delivery_time; // Total time taken for deliveries
} DeliveryPerson;

typedef struct {
    int id;
    int is_busy;
} Cook;

typedef struct {
    Order orders[MAX_ORDERS];
    int order_count;
    int prepared_count;
    Cook* cooks;
    DeliveryPerson* delivery_persons;
    int cook_count;
    int delivery_count;
    int delivery_speed;
    int total_deliveries;
    int total_delivery_time;
    int active_clients;
    int client_socket;
    pid_t client_pid; // Add client PID
    int cancel_flag; // Add a flag to indicate order cancellation
} PideShop;

PideShop shop;
pthread_mutex_t order_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t preparation_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t order_available_cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t preparation_complete_cond = PTHREAD_COND_INITIALIZER;
sem_t shovel_sem;
pthread_mutex_t oven_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t log_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t delivery_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t delivery_queue_cond = PTHREAD_COND_INITIALIZER;

int server_fd;
int client_socket;
pthread_t *cook_threads;
pthread_t *delivery_threads;
pthread_t cancel_thread;

void initialize_shop(int cook_count, int delivery_count, int delivery_speed);
void cleanup_shop();
void* cook_function(void* arg);
void* delivery_function(void* arg);
void* manager_function(void* arg);
void signal_handler(int signal);
void log_event(const char* event);
void promote_most_efficient_delivery_person();
void cancel_orders();
void stop_all_threads();
void stop_operations();
void check_all_orders_finished(); // Function prototype declaration
void* cancel_check_function(void* arg);
void reset_shop();

int main(int argc, char* argv[]) {
    if (argc != 5) {
        fprintf(stderr, "Usage: %s [portnumber] [CookthreadPoolSize] [DeliveryPoolSize] [speed]\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    int port = atoi(argv[1]);
    int cook_thread_pool_size = atoi(argv[2]);
    int delivery_thread_pool_size = atoi(argv[3]);
    int delivery_speed = atoi(argv[4]);

    initialize_shop(cook_thread_pool_size, delivery_thread_pool_size, delivery_speed);

    cook_threads = malloc(cook_thread_pool_size * sizeof(pthread_t));
    for (int i = 0; i < cook_thread_pool_size; i++) {
        pthread_create(&cook_threads[i], NULL, cook_function, (void*)&shop.cooks[i]);
    }

    delivery_threads = malloc(delivery_thread_pool_size * sizeof(pthread_t));
    for (int i = 0; i < delivery_thread_pool_size; i++) {
        pthread_create(&delivery_threads[i], NULL, delivery_function, (void*)&shop.delivery_persons[i]);
    }

    struct sockaddr_in address;
    int opt = 1;
    int addrlen = sizeof(address);

    // Create socket file descriptor
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("socket failed");
        cleanup_shop();
        exit(EXIT_FAILURE);
    }

    // Forcefully attaching socket to the port
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt))) {
        perror("setsockopt");
        cleanup_shop();
        close(server_fd);
        exit(EXIT_FAILURE);
    }
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    // Bind the socket to the network address and port
    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) {
        perror("bind failed");
        cleanup_shop();
        close(server_fd);
        exit(EXIT_FAILURE);
    }

    // Listen for incoming connections
    if (listen(server_fd, 3) < 0) {
        perror("listen");
        cleanup_shop();
        close(server_fd);
        exit(EXIT_FAILURE);
    }

    printf("PideShop active, waiting for connections on port %d...\n", port);

    // Set up signal handling for graceful shutdown using sigaction
    struct sigaction sa;
    sa.sa_handler = signal_handler;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGQUIT, &sa, NULL);

    if (sigaction(SIGINT, &sa, NULL) == -1) {
        perror("Error setting up SIGINT handler");
        exit(EXIT_FAILURE);
    }

    if (sigaction(SIGQUIT, &sa, NULL) == -1) {  // Changed to SIGTERM from SIGQUIT
        perror("Error setting up SIGTERM handler");
        exit(EXIT_FAILURE);
    }

    while (1) {
        client_socket = accept(server_fd, (struct sockaddr*)&address, (socklen_t*)&addrlen);
        if (client_socket >= 0) {
            printf("Connection accepted from @%s:%d\n", inet_ntoa(address.sin_addr), ntohs(address.sin_port));
            log_event("Client connection accepted");

            shop.client_socket = client_socket;
            shop.active_clients++;

            pthread_t manager_thread;
            pthread_create(&manager_thread, NULL, manager_function, (void*)&client_socket);
            pthread_detach(manager_thread); // Detach the thread to allow it to run independently

            pthread_create(&cancel_thread, NULL, cancel_check_function, NULL);
            pthread_detach(cancel_thread);
        } else {
            perror("accept");
        }
    }

    cleanup_shop();
    close(server_fd);
    close(client_socket);
    return 0;
}

void initialize_shop(int cook_count, int delivery_count, int delivery_speed) {
    shop.order_count = 0;
    shop.prepared_count = 0;
    shop.cook_count = cook_count;
    shop.delivery_count = delivery_count;
    shop.delivery_speed = delivery_speed;
    shop.total_deliveries = 0;
    shop.total_delivery_time = 0;
    shop.active_clients = 0;
    shop.client_pid = 0;
    shop.cancel_flag = 0; // Initialize the cancel flag

    shop.cooks = (Cook*)malloc(cook_count * sizeof(Cook));
    for (int i = 0; i < cook_count; i++) {
        shop.cooks[i].id = i;
        shop.cooks[i].is_busy = 0;
    }

    shop.delivery_persons = (DeliveryPerson*)malloc(delivery_count * sizeof(DeliveryPerson));
    for (int i = 0; i < delivery_count; i++) {
        shop.delivery_persons[i].id = i;
        shop.delivery_persons[i].speed = delivery_speed;
        shop.delivery_persons[i].order_count = 0;
        shop.delivery_persons[i].deliveries_made = 0;
        shop.delivery_persons[i].total_delivery_time = 0;
        memset(shop.delivery_persons[i].current_orders, -1, MAX_DELIVERY_ORDERS * sizeof(int));
    }

    sem_init(&shovel_sem, 0, MAX_SHOVELS);
}

void cleanup_shop() {
    free(shop.cooks);
    free(shop.delivery_persons);
    sem_destroy(&shovel_sem);
    pthread_mutex_destroy(&order_mutex);
    pthread_mutex_destroy(&preparation_mutex);
    pthread_mutex_destroy(&oven_mutex);
    pthread_mutex_destroy(&log_mutex);
    pthread_mutex_destroy(&delivery_queue_mutex);
    pthread_cond_destroy(&order_available_cond);
    pthread_cond_destroy(&preparation_complete_cond);
    pthread_cond_destroy(&delivery_queue_cond);
    free(cook_threads);
    free(delivery_threads);
}

void reset_shop() {
    pthread_mutex_lock(&order_mutex);
    shop.order_count = 0;
    shop.prepared_count = 0;
    shop.cancel_flag = 0; // Reset the cancel flag
    pthread_mutex_unlock(&order_mutex);
}

void* manager_function(void* arg) {
    client_socket = *(int*)arg;
    char buffer[BUFFER_SIZE] = {0};

    while (1) {
        int valread = read(client_socket, buffer, BUFFER_SIZE);

        if (valread <= 0) {
            printf("Client disconnected.\n");
            close(client_socket);
            shop.active_clients--;
            reset_shop(); // Reset the shop for new clients
            return NULL;
        }

        buffer[valread] = '\0';

        // Parse the client input including the PID
        int number_of_clients, p, q;
        sscanf(buffer, "%d:%d:%d:%d", &shop.client_pid, &number_of_clients, &p, &q);
        printf("PID: %d\n", shop.client_pid);

        printf("%d new customers.. Serving\n", number_of_clients);
        log_event("Received new customer orders");

        // Randomly generate customer positions within the town (p x q)
        srand(time(NULL));
        pthread_mutex_lock(&order_mutex);
        for (int i = 0; i < number_of_clients; i++) {
            if (shop.cancel_flag) break; // Stop processing if cancel flag is set

            Order new_order;
            new_order.order_id = shop.order_count;
            new_order.customer_id = i;
            new_order.x = (rand() % p) - (p / 2);
            new_order.y = (rand() % q) - (q / 2);
            new_order.status = 0;

            shop.orders[shop.order_count++] = new_order;
            printf("Order %d for customer %d at location (%d, %d)\n", new_order.order_id, new_order.customer_id, new_order.x, new_order.y);
            log_event("Order placed");
        }
        pthread_mutex_unlock(&order_mutex);

        // Signal cooks that orders are available
        pthread_cond_broadcast(&order_available_cond);

        // Wait for all orders to be finished
        pthread_mutex_lock(&order_mutex);
        while (shop.order_count > 0 && !shop.cancel_flag) {
            pthread_mutex_unlock(&order_mutex);
            sleep(1);
            pthread_mutex_lock(&order_mutex);
        }
        pthread_mutex_unlock(&order_mutex);

        // Signal client that all orders are finished
        if (!shop.cancel_flag) {
            send(client_socket, "All customers served", strlen("All customers served"), 0);
            log_event("All customers served");
            printf("> done serving client %d\n", shop.client_pid);
        } else {
            send(client_socket, "Order cancelled", strlen("Order cancelled"), 0);
            log_event("Order cancelled");
            printf("> cancelled serving client %d\n", shop.client_pid);
            reset_shop(); // Reset the shop for new clients
        }
    }
    return NULL;
}

void* cook_function(void* arg) {
    Cook* cook = (Cook*)arg;
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
    while (1) {
        pthread_mutex_lock(&order_mutex);
        while (shop.order_count == 0 && !shop.cancel_flag) {
            pthread_cond_wait(&order_available_cond, &order_mutex);
        }
        if (shop.cancel_flag) {
            pthread_mutex_unlock(&order_mutex);
            continue;
        }

        // Get the next order to prepare
        int order_index = -1;
        for (int i = 0; i < shop.order_count; i++) {
            if (shop.orders[i].status == 0) { // Order is placed but not yet prepared
                order_index = i;
                shop.orders[i].status = 1; // Mark as being prepared
                break;
            }
        }

        pthread_mutex_unlock(&order_mutex);

        if (order_index != -1) {
            // Simulate preparation time
            printf("Cook %d is preparing order %d\n", cook->id, shop.orders[order_index].order_id);
            log_event("Order is being prepared");
            for (int i = 0; i < 5; i++) { // Check cancel flag during preparation
                pthread_mutex_lock(&order_mutex);
                if (shop.cancel_flag) {
                    pthread_mutex_unlock(&order_mutex);
                    break;
                }
                pthread_mutex_unlock(&order_mutex);
                sleep(1);
            }

            if (shop.cancel_flag) continue; // Stop further cooking if cancel flag is set

            // Wait for an available shovel and oven space
            sem_wait(&shovel_sem);
            pthread_mutex_lock(&oven_mutex);

            // Simulate placing in the oven
            printf("Cook %d is placing order %d in the oven\n", cook->id, shop.orders[order_index].order_id);
            log_event("Order is being cooked");
            for (int i = 0; i < 3; i++) { // Check cancel flag during cooking
                pthread_mutex_lock(&order_mutex);
                if (shop.cancel_flag) {
                    pthread_mutex_unlock(&order_mutex);
                    break;
                }
                pthread_mutex_unlock(&order_mutex);
                sleep(1);
            }
            shop.orders[order_index].status = 2; // Mark as cooked

            pthread_mutex_unlock(&oven_mutex);
            sem_post(&shovel_sem);

            // Wait for an available shovel to remove from the oven
            sem_wait(&shovel_sem);
            pthread_mutex_lock(&oven_mutex);

            // Simulate removing from the oven
            printf("Cook %d is removing order %d from the oven\n", cook->id, shop.orders[order_index].order_id);
            log_event("Order is cooked");
            for (int i = 0; i < 2; i++) { // Check cancel flag during removal
                pthread_mutex_lock(&order_mutex);
                if (shop.cancel_flag) {
                    pthread_mutex_unlock(&order_mutex);
                    break;
                }
                pthread_mutex_unlock(&order_mutex);
                sleep(1);
            }

            // Notify that the order is ready for delivery
            pthread_mutex_lock(&preparation_mutex);
            shop.orders[order_index].status = 3; // Mark as ready for delivery
            shop.prepared_count++;
            pthread_cond_signal(&preparation_complete_cond);
            pthread_mutex_unlock(&preparation_mutex);

            pthread_mutex_unlock(&oven_mutex);
            sem_post(&shovel_sem);

            printf("Cook %d has finished order %d\n", cook->id, shop.orders[order_index].order_id);
        }
    }

    return NULL;
}

void* delivery_function(void* arg) {
    DeliveryPerson* delivery_person = (DeliveryPerson*)arg;
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
    while (1) {
        pthread_mutex_lock(&preparation_mutex);
        while (shop.prepared_count == 0 && !shop.cancel_flag) {
            pthread_cond_wait(&preparation_complete_cond, &preparation_mutex);
        }
        if (shop.cancel_flag) {
            pthread_mutex_unlock(&preparation_mutex);
            continue;
        }

        // Collect orders for delivery
        int orders_to_deliver[MAX_DELIVERY_ORDERS];
        int count = 0;

        pthread_mutex_lock(&order_mutex);
        for (int i = 0; i < shop.order_count && count < MAX_DELIVERY_ORDERS; i++) {
            if (shop.orders[i].status == 3) { // Order is ready for delivery
                orders_to_deliver[count++] = i;
                shop.orders[i].status = 4; // Mark as in delivery
            }
        }
        shop.prepared_count -= count;
        pthread_mutex_unlock(&order_mutex);

        pthread_mutex_unlock(&preparation_mutex);

        if (count > 0) {
            // Deliver orders
            int total_time = 0;
            for (int i = 0; i < count; i++) {
                pthread_mutex_lock(&order_mutex);
                if (shop.cancel_flag) {
                    pthread_mutex_unlock(&order_mutex);
                    break; // Stop further delivery if cancel flag is set
                }
                pthread_mutex_unlock(&order_mutex);

                int order_index = orders_to_deliver[i];
                Order* order = &shop.orders[order_index];
                int delivery_time = (abs(order->x) + abs(order->y)) / delivery_person->speed;

                printf("Delivery person %d is delivering order %d to customer %d at (%d, %d)\n",
                       delivery_person->id, order->order_id, order->customer_id, order->x, order->y);

                sleep(delivery_time); // Simulate delivery time
                total_time += delivery_time;

                printf("Delivery person %d has delivered order %d to customer %d\n",
                       delivery_person->id, order->order_id, order->customer_id);

                log_event("Order is delivered");

                order->status = 5; // Mark as delivered
                delivery_person->deliveries_made++;
                delivery_person->total_delivery_time += delivery_time;
            }

            // Return to shop
            sleep(total_time); // Simulate return time
            delivery_person->total_delivery_time += total_time;

            pthread_mutex_lock(&log_mutex);
            shop.total_deliveries += count;
            shop.total_delivery_time += total_time;
            pthread_mutex_unlock(&log_mutex);

            check_all_orders_finished();
        }
    }

    return NULL;
}

void signal_handler(int signal) {
    printf("Shutting down server...\n");
    log_event("Server shutting down");

    // Notify connected clients
    if (shop.active_clients > 0) {
        send(shop.client_socket, "shutdown", strlen("shutdown"), 0);
    }

    stop_all_threads();
    close(server_fd);
    cleanup_shop();
    exit(EXIT_SUCCESS);
}

void log_event(const char* event) {
    pthread_mutex_lock(&log_mutex);
    FILE* log_file = fopen("pide_shop_log.txt", "a");
    if (log_file) {
        fprintf(log_file, "%s\n", event);
        fclose(log_file);
    }
    pthread_mutex_unlock(&log_mutex);
}

void promote_most_efficient_delivery_person() {
    int best_person_id = -1;
    float best_efficiency = -1;

    for (int i = 0; i < shop.delivery_count; i++) {
        DeliveryPerson* dp = &shop.delivery_persons[i];
        if (dp->deliveries_made > 0) {
            float efficiency = (float)dp->deliveries_made / dp->total_delivery_time;
            if (efficiency > best_efficiency) {
                best_efficiency = efficiency;
                best_person_id = dp->id;
            }
        }
    }

    if (best_person_id != -1) {
        printf("Delivery person %d is promoted as the most efficient\n", best_person_id);
        log_event("Most efficient delivery person promoted");
    }
}

void cancel_orders() {
    pthread_mutex_lock(&order_mutex);

    for (int i = 0; i < shop.order_count; i++) {
        if (shop.orders[i].status == 1 || shop.orders[i].status == 2 || shop.orders[i].status == 3 || shop.orders[i].status == 4) {
            shop.orders[i].status = -1; // Mark order as cancelled
            printf("Order %d for customer %d cancelled.\n", shop.orders[i].order_id, shop.orders[i].customer_id);
            log_event("Order cancelled");
        }
    }

    pthread_mutex_unlock(&order_mutex);

    // Notify cooks and delivery personnel about the cancellation
    pthread_cond_broadcast(&order_available_cond);
    pthread_cond_broadcast(&preparation_complete_cond);
}

void check_all_orders_finished() {
    pthread_mutex_lock(&order_mutex);
    int all_orders_finished = 1;
    for (int i = 0; i < shop.order_count; i++) {
        if (shop.orders[i].status != 5 && shop.orders[i].status != -1) {
            all_orders_finished = 0;
            break;
        }
    }
    pthread_mutex_unlock(&order_mutex);

    if (all_orders_finished) {
        pthread_mutex_lock(&order_mutex);
        shop.order_count = 0;
        pthread_mutex_unlock(&order_mutex);
    }
}

void stop_all_threads() {
    for (int i = 0; i < shop.cook_count; i++) {
        pthread_cancel(cook_threads[i]);
    }
    for (int i = 0; i < shop.delivery_count; i++) {
        pthread_cancel(delivery_threads[i]);
    }
}

void stop_operations() {
    shop.cancel_flag = 1;
    cancel_orders();
}

void* cancel_check_function(void* arg) {
    while (1) {
        char buffer[BUFFER_SIZE] = {0};
        int valread = read(client_socket, buffer, BUFFER_SIZE);
        if (valread > 0 && strcmp(buffer, "CANCEL") == 0) {
            printf("Order cancellation received. Cancelling orders...\n");
            log_event("Order cancellation received");
            pthread_mutex_lock(&order_mutex);
            shop.cancel_flag = 1; // Set cancel flag
            stop_operations();
            pthread_mutex_unlock(&order_mutex);
            pthread_cond_broadcast(&order_available_cond); // Wake up all threads to check cancel flag
            pthread_cond_broadcast(&preparation_complete_cond);
        }
    }
    return NULL;
}
