#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

#define BUFFER_SIZE 1024

int server_socket;
pid_t pid;
void signal_handler(int signal);
void connect_to_server(const char* server_ip, int port, int number_of_clients, int p, int q);
void cleanup();
void handle_shutdown();

int main(int argc, char* argv[]) {
    if (argc != 5 && argc != 6) {  // Accept either 5 or 6 arguments
        fprintf(stderr, "Usage: %s [server_ip] [portnumber] [numberOfClients] [p] [q]\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    const char* server_ip = "127.0.0.1";  // Default to local IP
    int port;
    int number_of_clients;
    int p, q;

    if (argc == 6) {
        server_ip = argv[1];
        port = atoi(argv[2]);
        number_of_clients = atoi(argv[3]);
        p = atoi(argv[4]);
        q = atoi(argv[5]);
    } else {  // argc == 5
        port = atoi(argv[1]);
        number_of_clients = atoi(argv[2]);
        p = atoi(argv[3]);
        q = atoi(argv[4]);
    }

    // Set up signal handling for graceful shutdown
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

    pid = getpid();

    connect_to_server(server_ip, port, number_of_clients, p, q);

    return 0;
}

void signal_handler(int signal) {
    if (signal == SIGINT || signal == SIGQUIT) {
        printf("^C signal received. Cancelling orders and exiting...\n");
        send(server_socket, "CANCEL", strlen("CANCEL"), 0);
        cleanup();
        exit(EXIT_SUCCESS);
    }
}

void handle_shutdown() {
    printf("Server shutdown message received. Cleaning up and exiting...\n");
    cleanup();

    // Kill all child processes
    pid_t child_pid = fork();
    if (child_pid == 0) {
        // Child process to kill other child processes
        if (pid != getpid()) {
            kill(getpid(), SIGKILL);
        }
        exit(EXIT_SUCCESS);
    } else {
        wait(NULL);
    }
    exit(EXIT_SUCCESS);
}

void connect_to_server(const char* server_ip, int port, int number_of_clients, int p, int q) {
    struct sockaddr_in server_addr;
    char buffer[BUFFER_SIZE];

    // Create socket
    if ((server_socket = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        perror("Socket creation error");
        exit(EXIT_FAILURE);
    }

    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);

    // Convert IPv4 and IPv6 addresses from text to binary form
    if (inet_pton(AF_INET, server_ip, &server_addr.sin_addr) <= 0) {
        perror("Invalid address or Address not supported");
        exit(EXIT_FAILURE);
    }

    // Connect to the server
    if (connect(server_socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        perror("Connection failed");
        exit(EXIT_FAILURE);
    }

    // Send client information to server
    snprintf(buffer, BUFFER_SIZE, "%d:%d:%d:%d", pid, number_of_clients, p, q);
    send(server_socket, buffer, strlen(buffer), 0);

    printf("Connected to server.\nPID: %d\n", getpid());

    while (1) {
        memset(buffer, 0, BUFFER_SIZE);
        int valread = read(server_socket, buffer, BUFFER_SIZE);

        if (valread == 0) {
            printf("Server closed the connection.\n");
            break;
        }

        buffer[valread] = '\0';
        printf("%s\n", buffer);

        if (strcmp(buffer, "shutdown") == 0) {
            handle_shutdown();
        } else if (strcmp(buffer, "All customers served") == 0) {
            printf("All customers served. Waiting for new client info...\n");
            printf("Enter new number of clients, p and q: ");
            while (scanf("%d %d %d", &number_of_clients, &p, &q) != 3 || number_of_clients <= 0 || p <= 0 || q <= 0) {
                fprintf(stderr, "Error: Please enter three positive integers for number of clients, p, and q.\n");
                while (getchar() != '\n'); // Clear input buffer
                printf("Enter new number of clients, p and q: ");
            }

            pid_t child_pid = fork();
            if (child_pid < 0) {
                perror("fork failed");
                exit(EXIT_FAILURE);
            } else if (child_pid == 0) {
                // Child process
                pid = getpid();
                connect_to_server(server_ip, port, number_of_clients, p, q);
                exit(EXIT_SUCCESS); // Exit child process after connection
            } else {
                // Parent process
                wait(NULL); // Wait for the child process to finish
            }
        }
    }

    cleanup();
}

void cleanup() {
    close(server_socket);
}
