#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/mman.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <semaphore.h>

#define clientSendFifo "/tmp/send_fifo_client.%d" //client uses to send info to server server will read from this fifo
#define clientGetFifo "/tmp/get_fifo_client.%d"
#define serverFifo "/tmp/neHos_server_fifo_%d"
#define BUF_SIZE 1024
//define gloabals
int max_clients;
int num_clients = 0;
int *clients;
int log_file_fd;
const char *dirname;
sem_t *sem_client;
char *semaphore_name;
int child_client_pid = -1;
int server_fifo_fd = -1;
int parent_pid;
char server_fifo_name[128];

typedef struct CustomNode
{
    int data;
    struct CustomNode *next;
} Node;

typedef struct CustomQueue
{
    Node *front;
    Node *rear;
} Queue;

Queue *queueConstructor()
{
    Queue *queue = (Queue *)malloc(sizeof(Queue));
    queue->front = NULL;
    queue->rear = NULL;
    return queue;
}

int checkEmpty(Queue *queue)
{
    return queue->front == NULL;
}

void addElement(Queue *queue, int data)
{
    Node *node = (Node *)malloc(sizeof(Node));
    node->data = data;
    node->next = NULL;

    if (checkEmpty(queue))
    {
        queue->front = node;
        queue->rear = node;
    }
    else
    {
        queue->rear->next = node;
        queue->rear = node;
    }
}

int getElement(Queue *queue)
{
    if (checkEmpty(queue))
    {
        return -1;
    }
    Node *temp = queue->front;
    int data = temp->data;
    queue->front = temp->next;
    if (queue->front == NULL)
    {
        queue->rear = NULL;
    }
    free(temp);
    return data;
}


Queue *queue;


typedef struct
{
    int *clients_arr;
    int client_nums;
    int client_count;
} ClientQueue;

ClientQueue *client_queue;

void check_args(int argc, char *argv[]);

int get_max_clients(char *argv[]);
int parse_input(char *user_input, char **user_commands, char delimiter);
char *remove_spaces(char *user_input);

void handle_signal(int signum);
void init_signal_handler();
void write_to_log_file(char *message);

int create_server_fifo();
void create_server_dir(const char *dirname);
void create_log_file();
void create_shared_memory();
int available_spots_in_queue(int *clients);

void print_server_info(const char *dirname);
void __list(int client_fifo_fd);
void _readF(int client_fifo_fd, char *prompt);
void _writeT(int client_fifo_fd, char *prompt);
void _upload(int client_fifo_fd, int client_get_fifo_fd, char *prompt);
void _download(int client_fifo_fd, char *prompt);
void _archServer(int client_fifo_fd, char *prompt);

void handle_connection_requests(int server_fifo_fd);

void handle_client_request(int client_pid, int idx, int client_id, int server_fifo_fd);

int connect_send_client_fifo(int client_pid);

int connect_get_client_fifo(int client_pid);

void handle_commands(int client_get_fifo_fd, int client_send_fifo_fd, char *command);

char **create_2d_array();
void free_2d_array(char **array);

void init_semaphore();
void close_semaphore();

int main(int argc, char *argv[])
{

    //if command line args correct, store provided into max_clients
    //max_clients used for declaring memory for the clients in the client queue dynamically
    check_args(argc, argv);
    max_clients =  get_max_clients(argv);
    //initialize a semaphore for handling synchronization among clients
    init_semaphore();
    //give pointer to sem object and parent_pid. return sem name

    dirname = argv[1];

    create_server_dir(dirname);

    create_log_file();

    create_shared_memory();

    init_signal_handler();

    print_server_info(dirname);

    int server_fifo_fd = create_server_fifo();

    handle_connection_requests(server_fifo_fd);
//cleanup
    close(server_fifo_fd);
    close_semaphore();
    unlink(server_fifo_name);

    return 0;
}

// Command Line Functions
void check_args(int argc, char *argv[])
{
    if (argc != 3)
    {
        fprintf(stderr, "Usage: %s <dirname> <max. #ofClients>\n", argv[0]);
        exit(1);
    }
}
/**
 * Read user input and separate it into commands based on the given delimeter parameter
 * @param user_input input string
 * @param user_commands a pointer to an pre allocated string array. 
 * Each element in the array will store a command that has been parsed from the `user_input` from `delimiter`. 
 * @param delimiter character to be used to separate the input into seperate commands.
 */
int parse_input(char *user_input, char **user_commands, char delimiter)
{
    char terminator = '\0';

    int i = 0;
    int k = 0;
    int z = 0;
    int _count =0;

    // Iterate through the input string
    while (user_input[i] != terminator)
    {
        // Check if the current character is a pipe or not
        if (user_input[i] != delimiter)
        {
            // assign char to command
            user_commands[z][k] = user_input[i];
            k++;
        }
        else // If the current character is a pipe
        {
            // Terminate the current command string
            user_commands[z][k] = terminator;

            // Delete the leading and trailing spaces from the command string
            user_commands[z] = remove_spaces(user_commands[z]);

            // Increment k and z
            z++;
            k = 0;
        }

        i++;
    }

    // For the last command
    user_commands[z][k] = terminator;
    user_commands[z] = remove_spaces(user_commands[z]);

    // Make the last element of the commands array NULL
    if (user_commands[z + 1] != NULL)
    {
        free(user_commands[z + 1]);
    }
    user_commands[z + 1] = NULL;
    return z+1;
}

char *remove_spaces(char *user_input)
{
    int i = 0;
    char terminator = '\0';
    char empty_char = ' ';

    // Find the first character that is not space
    while (user_input[i] == empty_char)
    {
        i++;
    }

    // Check if there is any leading space or not
    if (i > 0)
    {
        // Delete the leading spaces by shifting
        int j = 0;
        while (user_input[i])
        {
            user_input[j] = user_input[i];
            i++;
            j++;
        }
        user_input[j] = terminator;
    }

    // Delete the trailing spaces
    int len_of_str = strlen(user_input);
    while (len_of_str > 0)
    {
        len_of_str--;

        // Check if the current character is a space or not
        if (user_input[len_of_str] != empty_char)
        {
            // Terminate string and break the loop
            user_input[len_of_str + 1] = terminator;
            break;
        }
    }

    return user_input;
}

int get_max_clients(char *argv[])
{
    int max_clients = atoi(argv[2]);
    if (max_clients == 0)
    {
        fprintf(stderr, "Usage: %s <dirname> <max. #ofClients>\n", argv[0]);
        exit(1);
    }
    return max_clients;
}

// create a directory with the orovided name
void create_server_dir(const char *dirname)
{
    char dir_path[256];
    sprintf(dir_path, "%s", dirname);

    if (mkdir(dir_path, 0777) == -1 && errno != EEXIST)
    {
        perror("Error while creating directory:");
        exit(1);
    }
}
//create the log file for the server. if file exist delete the content
void create_log_file()
{
    char log_path [256];
    sprintf(log_path, "%s/server_log.txt", dirname);

    log_file_fd = open(log_path, O_CREAT | O_WRONLY | O_TRUNC, 0666);
    if (log_file_fd == -1)
    {
        perror("Error in open");
        exit(1);
    }
}

// Signal Handler 
void handle_signal(int signum)
{
    if (parent_pid == getpid())
    {
        for (int i = 0; i < max_clients; i++)
        {
            //kill client processes in the queue
            if (client_queue->clients_arr[i] != 0 && client_queue->clients_arr[i] != -1)
            {
                kill(client_queue->clients_arr[i], SIGINT);
            }
        }

        // kill all processes in the queue
        while (queue != NULL && !checkEmpty(queue))
        {
            int client_pid = getElement(queue);
            kill(client_pid, SIGINT);
        }

        close(log_file_fd);

        sem_close(sem_client);

        sem_unlink(semaphore_name);

        printf(">> bye\n");
        printf(">\n");
    }

    exit(0);
}

/**
 * signal initializer for the signal handlers for SIGINT, SIGTERM, SIGTSTP, and SIGQUIT signals.
 */
void init_signal_handler()
{
    struct sigaction sigact;

    sigact.sa_handler = handle_signal;

    sigemptyset(&sigact.sa_mask);

    sigact.sa_flags = 0;

    sigaction(SIGINT, &sigact, NULL);

    sigaction(SIGTERM, &sigact, NULL);

    sigaction(SIGTSTP, &sigact, NULL);

    sigaction(SIGQUIT, &sigact, NULL);
}

// display serve info
void print_server_info(const char *dirname)
{
    printf("> neHosServer %s %d\n\n", dirname, max_clients);
    printf(">> Server Started PID %d...\n", getpid());
    printf(">> waiting for clients...\n");
}

/**
 * This function checks if there are available spots in an array of clients.
 
 */
int available_spots_in_queue(int *clients)
{
    int i;
    for (i = 0; i < max_clients; i++)
    {
        if (clients[i] == 0)
        {
            return 1;
        }
    }
    return 0;
}
/**
 * create a shared memory region that can be accessed by multiple processes
 * store `ClientQueue` struct and initialize it with memory allocation for client data.
 */
void create_shared_memory()
{
    // Create a shared memory region to store the ClientQueue struct
    int shm_fd = shm_open("/neHos_server_shm", O_CREAT | O_RDWR, 0666);
    if (shm_fd < 0)
    {
        perror("Error in shm_open");
        exit(1);
    }
    // set the size of the shared memory.
    //the desired size of the memory region (sizeof(ClientQueue) + max_clients * sizeof(int)
    if (ftruncate(shm_fd, sizeof(ClientQueue) + max_clients * sizeof(int)) < 0)
    {
        perror("Error in ftruncate");
        exit(1);
    }


    // Map the shared memory to the address space of the calling process using mmap()
    /*  mmap takes: the starting address (NULL for the system to choose a suitable address), 
                    the size of the shared memory region
                    protection flags (PROT_READ | PROT_WRITE for allowing read and write access)
                    sharing flags (MAP_SHARED for sharing the memory with other processes)
                    the file descriptor of the shared memory object (shm_fd)
                    the offset within the shared memory object (0) */    
    client_queue = (ClientQueue *)mmap(NULL, sizeof(ClientQueue) + max_clients * sizeof(int),
                                   PROT_READ | PROT_WRITE,
                                   MAP_SHARED, shm_fd, 0);
    if (client_queue == MAP_FAILED)
    {
        perror("Error in mmap");
        exit(1);
    }

    // Allocate memory for the clients_arr
    client_queue->clients_arr = (int *)((char *)client_queue + sizeof(ClientQueue));

    // Initialize the memory
    memset(client_queue->clients_arr, 0, max_clients * sizeof(int));
    client_queue->client_nums = 0;
    client_queue->client_count = 0;
}


/**
 * allocates memory for a 2D array of strings to store commands
 * 
 * @return return a double pointer that points to an array of strings (commands) where each string can hold up to 255 characters. 
 */
char **create_2d_array()
{
    // create an array of strings to store the commands
    char **commands = malloc((BUF_SIZE) * sizeof(char *));

    // check if malloc is successful
    if (commands == NULL)
    {
        fprintf(stderr, "There is something wrong with malloc");
    }

    for (int i = 0; i < BUF_SIZE; i++)
    {
        // allocate memory for each command
        commands[i] = malloc(256 * sizeof(char));

        // check if malloc is successful
        if (commands[i] == NULL)
        {
            fprintf(stderr, "There is something wrong with malloc");
        }
    }

    // make the last element of the commands array NULL
    commands[BUF_SIZE] = NULL;
    return commands;
}
void free_2d_array(char **array)
{
    if (array == NULL)
        return;

    // Free each individual string
    for (int i = 0; array[i] != NULL; i++)
    {
        free(array[i]);
    }

    // Free the array of pointers
    free(array);
}

void write_to_log_file(char *message)
{
    // write to the log file
    if (write(log_file_fd, message, strlen(message)) < 0)
    {
        perror("Error in write to log file");
        exit(1);
    }
}
void init_semaphore()
{
    semaphore_name = (char *)malloc(sizeof(char) * 256);
    sprintf(semaphore_name, "/semaphore_%d", getpid());
    sem_client = sem_open(semaphore_name, O_CREAT, 0644, 0);
    parent_pid = getpid();
}
void close_semaphore()
{
    sem_close(sem_client);
    sem_unlink(semaphore_name);
}



int connect_send_client_fifo(int client_pid)
{
    // Create the FIFO to send output to the client
    char client_send_fifo_name[128];
    sprintf(client_send_fifo_name, clientSendFifo, client_pid);

    // Open the FIFO to send output to the client
    int client_send_fifo_fd = open(client_send_fifo_name, O_RDONLY);
    if (client_send_fifo_fd < 0)
    {
        perror("Error in open send fifo");
        exit(1);
    }

    return client_send_fifo_fd;
}


int connect_get_client_fifo(int client_pid)
{
    // Create the FIFO to get input from the client
    char client_get_fifo_name[128];
    sprintf(client_get_fifo_name, clientGetFifo, client_pid);

    // Open the FIFO to get input from the client
    int client_get_fifo_fd = open(client_get_fifo_name, O_WRONLY);
    if (client_get_fifo_fd < 0)
    {
        perror("Error in open get fifo");
        exit(1);
    }

    return client_get_fifo_fd;
}

int create_server_fifo()
{

    sprintf(server_fifo_name, serverFifo, getpid());
    printf("serpid: %s\n", server_fifo_name);
    // Create a FIFO to receive connection requests from clients
    if (mkfifo(server_fifo_name, 0666) == -1 && errno != EEXIST)
    {
        perror("Error in mkfifo");
        exit(1);
    }

    // Open the FIFO to receive connection requests from clients
    int server_fifo_fd = open(server_fifo_name, O_RDONLY);
    if (server_fifo_fd < 0)
    {
        perror("Error in open server fifo");
        exit(1);
    }

    return server_fifo_fd;
}

// Client Commands Execution Functions are implemented below


/**
 * __list creates a child process to execute the ls command in a specified directory and sends the output to a client FIFO file descriptor.
 * 
 * @param client_fifo_fd  the file descriptor for a FIFO that is used for communication between processes.
 */
void __list(int client_fifo_fd)
{
    int pipe_fd[2];
    if (pipe(pipe_fd) < 0)
    {
        perror("Error in pipe");
        exit(1);
    }

    pid_t child_pid = fork();
    if (child_pid == 0)
    {
        close(pipe_fd[0]);

        dup2(pipe_fd[1], STDOUT_FILENO);

        execlp("ls", "ls", dirname, NULL);

        close(pipe_fd[1]);
        exit(0);
    }

    close(pipe_fd[1]);

    char buffer[BUF_SIZE];
    int bytes_read = 0;
    while ((bytes_read = read(pipe_fd[0], buffer, sizeof(buffer))) > 0)
    {
        write(client_fifo_fd, buffer, sizeof(buffer));
    }

    close(pipe_fd[0]);
}

/**
 *  _readF function reads and sends specific lines or the entire content of a file to a client based on the client input provided.
 * 
 * @param client_fifo_fd  the file descriptor for the client FIFO This file descriptor is used 
 * to communicate with the client by reading from or writing to the FIFO pipe.
 * @param prompt string contain client command input. 
 * The client input is splitted from ' ' and file name, # of line info extracted into tokens string array

 */
void _readF(int client_fifo_fd, char *prompt)
{
    int line_number = -1;

    char **tokens = create_2d_array();
    //if line number provided token count3 , else 2
    int token_count = parse_input(prompt, tokens, ' ');
    if (tokens[1] == NULL)
    {
        write(client_fifo_fd, "readF wrong format", BUF_SIZE);
        return;
    }
    printf("token1:%s, token2:%s\n",tokens[1], tokens[2]);

    // if line number is provided, convert it to integer
    if (tokens[2] != NULL)
    {
        line_number = atoi(tokens[2]);
        if (line_number == 0 || line_number < 0)
        {
            write(client_fifo_fd, "Invalid line number", BUF_SIZE);
            return;
        }
        line_number--;
    }
    printf("line%d\n", line_number);

    char file_path[128];

    sprintf(file_path, "%s/%s", dirname, tokens[1]);

    // open the file using open() system call
    int file_fd = open(file_path, O_RDONLY);
    if (file_fd < 0)
    {
        perror("Error in open file");
        write(client_fifo_fd, "open failed", BUF_SIZE);
        return;
    }

    write(client_fifo_fd, "Y", BUF_SIZE);

    char line[BUF_SIZE];
    int current_line = 0;
    char c;
    int index = 0;
    int n;

    // read the file contents using read() system call
    while ((n = read(file_fd, &c, 1)) > 0)
    {
        if(c == '\0') //end of line
        {
            line[index] = '\0';
            write(client_fifo_fd, line, BUF_SIZE);
            break;
        }
        else if (c == '\n' ) //  or file
        {
            line[index] = '\0'; // null-terminate the line

            // if line number is not provided, send all lines
            if (line_number == -1)
            {
                write(client_fifo_fd, line, BUF_SIZE);
                memset(line, 0, BUF_SIZE);
                write(1, line, BUF_SIZE);
            }
            // if line number is provided, send only that line
            else if (current_line == line_number)
            {
                write(client_fifo_fd, line,  BUF_SIZE);
                memset(line, 0, BUF_SIZE);
                break;
            }

            current_line++;
            index = 0; // reset the index for the next line
        }
        else if (index < BUF_SIZE-1) // to prevent buffer overflow
        {
            write(1, &c, 1);
            line[index++] = c;
        }
    }

    // write the last line if line number is not provided
    if (line_number == -1)
    {
        // check if the last character is a not null or newline character
        if (line[index - 1] != '\0' && line[index - 1] != '\n')
        {
            line[index] = '\0';
            write(client_fifo_fd, line, BUF_SIZE);
        }
    }
    memset(line, 0, BUF_SIZE);
 
    if (line_number > current_line)
    {
        write(client_fifo_fd, "Out of file size!!", BUF_SIZE);
        write(client_fifo_fd, "N", BUF_SIZE);

        close(file_fd);
        return;
    }
    write(client_fifo_fd, "XX", BUF_SIZE);
    // Close the file
    close(file_fd);
    free_2d_array(tokens);
}

//will read from client send the message sent from client containing

/**
 * `_writeT` parses input commands, opens a file for writing. 
 * If number of line for writing provided, it seeks to a specified line writes a string to that line, and sends success messages to the client.
 * If number of line is not provided by client it writes the input string to the end of file amd close it.
 * @param client_fifo_fd  the file descriptor for the client FIFO 
 * @param prompt client input in the `writeT <file> <line #> <string>` format
 */
void _writeT(int client_fifo_fd, char *prompt)
{
    // Parse input command
    char **tokens = create_2d_array();
    //split the prompt into string array in tokens
    // prompt format writeT <file> <line #> <string>
    int token_count =  parse_input(prompt, tokens, ' ');
    // Construct file path
    char file_path[256];
    sprintf(file_path, "%s/%s", dirname, tokens[1]);

    // Open the file for writing
    int file_fd = open(file_path,  O_RDWR | O_CREAT, 0666);
    if (file_fd < 0) {
        perror("Error opening file");
        write(client_fifo_fd, "open failed", BUF_SIZE);
        return;
    }

    // if at least file naem and string provided
    if (token_count <3) {
        write(client_fifo_fd, "command format: writeT <file> <line #> <string>", BUF_SIZE);
        return;
    }

    
    int current_line = 1;
    int line_number = -1;
    int str_len = strlen(tokens[token_count-1]);
    char *str_content =tokens[token_count-1];

    // char str_content[str_len+4];
    // sprintf(str_content, "%s%s", tokens[token_count-1], "\n");

    // If line number is provided, seek to that line
    if (token_count == 4) {
        int line_number = atoi(tokens[2]);
                
        if (line_number > 0) {
            char ch;
            off_t offset = 0;

            // Read the file line by line until the specified line number
            while (read(file_fd, &ch, 1) != 0) {
                if (ch == '\n')
                    current_line++;
                if (current_line == line_number)
                    break;
                offset++;
            }
            if (current_line < line_number){  
                offset = lseek(file_fd, 0, SEEK_END) ; 
                if (offset ==-1){
                    perror("Error in lseek");
                    close(file_fd);
                    write(client_fifo_fd, "Lseek failed", BUF_SIZE);
                    return;
                }         
            }
        }else{
            perror("Error: invalid line nubmer");
            close(file_fd);
            write(client_fifo_fd, "Error: invalid line nubmer", BUF_SIZE);
            return;
        }
        // Write the string to the file
        if (write(file_fd, str_content, str_len) < 0) {
            perror("Error writing to file");
            write(client_fifo_fd, "writing to file failed", BUF_SIZE);
            close(file_fd);
            return;
        }
    }
    else {
        // Set the offset to the end of the file if no line number is provided
        int ending = lseek(file_fd, 0, SEEK_END) ; 
        if (ending ==-1){
            perror("Error in lseek");
            close(file_fd);
            write(client_fifo_fd, "N", BUF_SIZE);
            return;
        }     
        write(file_fd,"\n", 1) ;
        // Write the string to the file
        if (write(file_fd, str_content, str_len) < 0) {
            perror("Error writing to file");
            write(client_fifo_fd, "writing to file failed", BUF_SIZE);
            close(file_fd);
            return;
        }
    }
    // Send success message to client
    write(client_fifo_fd, "Y", BUF_SIZE);
    // Close the file
    close(file_fd);
    free_2d_array(tokens);
}

void _upload(int client_send_fifo_fd, int client_get_fifo_fd,char *prompt)
{
    char **tokens = create_2d_array();

    parse_input(prompt, tokens, ' ');

    if (tokens[1] == NULL)
    {
        write(client_send_fifo_fd, "Error: No file name provided\n", BUF_SIZE);
        printf("No file name\n");
        return;
    }

    char file_path [256];

    sprintf(file_path, "%s/%s", dirname, tokens[1]);

    int file_fd = open(file_path, O_WRONLY | O_CREAT | O_TRUNC, 0666); // Open or create the file for writing
    if (file_fd == -1)
    {
        char error_message[1024];
        sprintf(error_message, "Error: Unable to open file '%s' for writing\n", file_path);
        return;
    }

    char buffer[BUF_SIZE];
    ssize_t bytes_read;
    ssize_t tot_bytes = 0;

    while ((bytes_read = read(client_send_fifo_fd, buffer, sizeof(buffer))) > 0)
    {
        if (buffer[bytes_read - 1] == 'X' && buffer[bytes_read - 2] == 'X')
        {
            buffer[bytes_read - 2] = '\0';
            write(file_fd, buffer, bytes_read - 2);
            memset(buffer, 0, sizeof(buffer));
            break;
        }
        tot_bytes += bytes_read;

        write(file_fd, buffer, bytes_read);
    }
    close(file_fd); // Close the server FIFO

    printf("File transfer success. %d bytes received\n",(int)tot_bytes);

}

void _download(int client_fifo_fd, char *prompt)
{
    char **user_commands = create_2d_array();

    parse_input(prompt, user_commands, ' ');

    if (user_commands[1] == NULL)
    {
        printf("Error: No filename specified\n");
        write(client_fifo_fd, "N", 2);
        return;
    }

    char *filename = user_commands[1];

    char file_path[256];
    sprintf(file_path, "%s/%s", dirname, filename);

    int file_fd = open(file_path, O_RDONLY);
    if (file_fd == -1)
    {
        printf("Error: File does not exist\n");
        write(client_fifo_fd, "N", 2);
        return;
    }

    if (write(client_fifo_fd, "Y", 2) == -1)
    {
        perror("Error in writing to client FIFO");
        return;
    }

    char buffer[BUF_SIZE];
    ssize_t total_bytes_read = 0;
    ssize_t bytes_read;

    while ((bytes_read = read(file_fd, buffer, BUF_SIZE)) > 0)
    {
        total_bytes_read += bytes_read;
        write(client_fifo_fd, buffer, bytes_read); // Write the read data to the server FIFO
    }
     // send something uniuqe to indicate end of file
    write(client_fifo_fd, "XX", 2);

    close(file_fd);
    printf("Sending file success. %d bytes has been transferred\n",(int)total_bytes_read);

}
int count_files_in_directory(const char *dir_path) {
    DIR *dir;
    struct dirent *entry;
    int file_count = 0;

    // Open the directory
    dir = opendir(dir_path);
    if (dir == NULL) {
        perror("opendir");
        exit(EXIT_FAILURE);
    }

    // Iterate through each entry in the directory
    while ((entry = readdir(dir)) != NULL) {
        char full_path[BUF_SIZE];
        struct stat st;

        // Construct the full path of the entry
        sprintf(full_path, "%s/%s", dir_path, entry->d_name);

        // Get information about the entry
        if (lstat(full_path, &st) == -1) {
            perror("lstat");
            continue; // Skip this entry
        }

        // Check if the entry is a regular file
        if (S_ISREG(st.st_mode)) {
            file_count++;
        }
    }

    // Close the directory
    closedir(dir);

    return file_count;
}

void _archServer(int client_fifo_fd, char *file_name){
    // Ensure filename ends with ".tar"
    if (file_name == NULL){
        printf("No filename name.\n");
        write(client_fifo_fd, "No filename name.\n", BUF_SIZE);
        write(client_fifo_fd, "N", BUF_SIZE);

        return;
    }
    if (strlen(file_name) < 5 || strcmp(&file_name[strlen(file_name) - 4], ".tar") != 0)
    {
        printf("Invalid filename format. Filename must end with \".tar\".\n");
        write(client_fifo_fd, "Invalid filename format. Filename must end with \".tar\".\n", BUF_SIZE);
        write(client_fifo_fd, "N", BUF_SIZE);
        return;
    }
    char res[256]; // Buffer for messages to clients

   

    // Create the full path of the directory where server working
    char dir_path[256];
    sprintf(dir_path,"./%s", dirname);
    int file_count = count_files_in_directory(dir_path);
     // Write operation status messages to the FIFO
    sprintf(res, "%s %d %s\n", "Archiving the current contents of the server...\nCreating archive directory...\n",file_count, "files downloaded...");
    write(client_fifo_fd, res, BUF_SIZE);

    //fork a child pcess for archiving process
    pid_t pid = fork();

    if (pid == -1)
    {
        // Fork failed
        perror("fork failed");
        exit(EXIT_FAILURE);
    }
    else if (pid == 0)
    {
        // define execvp function arguments and provide server working directory that has been created
        char *exec_args[] = {"tar", "-cvf", file_name, dir_path, NULL};
        execvp("tar", exec_args);
        // execvp only returns if an error occurs
        perror("execvp");
        exit(EXIT_FAILURE);
    }
    else
    {
        // Parent process
        int status;
        waitpid(pid, &status, 0);
        // send response of the archiving command to client
        if (WIFEXITED(status) && WEXITSTATUS(status) == 0){
            
            sprintf(res, "%s%s\n", "SUCCESS Server side files are achived in>>", file_name);
            printf("%s",res);
            write(client_fifo_fd, res, BUF_SIZE);
            write(client_fifo_fd, "Y", BUF_SIZE);
        }
        else{
            printf("Error occurred while creating the archive.\n");
            write(client_fifo_fd, "Error occurred while creating the archive.\n", BUF_SIZE);
            write(client_fifo_fd, "N", BUF_SIZE);
        }
    }
}
/**
 * Manages incoming connection requests from clients, checks
 * if the queue is full, and adds clients to the queue for further processing.
 * 
 * @param server_fifo_fd the file descriptor for the server FIFO. It's used to
 *  read messages from the server FIFO where clients send their connection requests. 
 */
void handle_connection_requests(int server_fifo_fd)
{
    //create a struct queue to store the clients
    queue = queueConstructor();

    while (1){
        int client_pid = -1;

        char pipe_message [128];


        sem_wait(sem_client);//wait for sem 

     
        if (read(server_fifo_fd, pipe_message, 128) > 0)
        {
            char connect_option[128];

            sscanf(pipe_message, "%d%s", &client_pid, connect_option);

            char log_message[128];

            sprintf(log_message, "Connection request received from PID %d\n", client_pid);

            write_to_log_file(log_message);

            if (available_spots_in_queue(client_queue->clients_arr) == 0){
                if (strcmp(connect_option, "tryConnect") == 0)
                {
                    int client_get_fifo_fd = connect_get_client_fifo(client_pid);
                    //send message to client to indicate that client queue is full
                    if (write(client_get_fifo_fd, "full", BUF_SIZE) < 0)
                    {
                        perror("Error while to client fifo");
                        exit(1);
                    }
                    printf(">> Connection request PID %d... Queue FULL\n", client_pid);

                    continue;
                }
                printf(">> Connection request PID %d... Queue FULL\n", client_pid);
            }
            else{
                if (strcmp(connect_option, "tryConnect") == 0) {
                    int client_get_fifo_fd = connect_get_client_fifo(client_pid);
                    if (write(client_get_fifo_fd, "Queue is not full", BUF_SIZE) < 0){
                        perror("Error in writing to client fifo");
                        exit(1);
                    }
                }
            }
            //add connected client to the queue 
            addElement(queue, client_pid);
        }

        if (available_spots_in_queue(client_queue->clients_arr) != 0){

            int newest_client = getElement(queue);
            if (newest_client == -1)
                continue;

            if (client_queue->client_nums < max_clients){
                int idx = 0;

                for (idx = 0; idx < max_clients; idx++){
                    if (client_queue->clients_arr[idx] == 0) {
                        client_queue->clients_arr[idx] = newest_client;
                        break;
                    }
                }

                client_queue->client_nums++;
                client_queue->client_count++;

                handle_client_request(newest_client, idx, client_queue->client_count, server_fifo_fd);
            }
            else
                printf(">> Connection request PID %d... Queue FULL\n", client_pid);
        }
    }
}

/**
 * used handles client requests by forking a child process to manage
 * communication with the client and execute according to client commands.
 * 
 * @param client_pid the process ID of the client that is making the request to the server. 
 * It's used to establish communication channels (FIFOs) between the server and the client for sending and receiving messages.
 * @param idx the index of the client in the client queue. used to acces the relevant client object in the queue
 * 
 * @param client_id  the identifier assigned to each client connected to the server. It is used to distinguish between
 * different clients 
 * @param server_fifo_fd  the file descriptor for the server FIFO pipe. This file descriptor is used to
 * communicate with the server FIFO, which is a named pipe for inter-process communication between the server and clients.
 */
void handle_client_request(int client_pid, int idx, int client_id, int server_fifo_fd)
{
    pid_t child_pid = fork();
    if (child_pid == 0)
    {
        close(server_fifo_fd);

        child_client_pid = client_pid;

        int client_get_fifo_fd = connect_get_client_fifo(client_pid);

        int client_send_fifo_fd = connect_send_client_fifo(client_pid);

        char client_name[128];

        sprintf(client_name, "client%02d", client_id);

        printf(">> Client PID %d connected as \"%s\"\n", client_pid, client_name);

        char client_input[BUF_SIZE];

        /* Continuously read input from a client through fifo that client uses to send the entered input. 
        It reads the client input, logs the command received with the client info, and then checks the input command. */
        while (1){
            //read client input comming through send fifo
            if (read(client_send_fifo_fd, client_input, sizeof(client_input)) > 0)
            {
                char log_message [BUF_SIZE+256];
                sprintf(log_message, "%s command received from %s\n", client_input, client_name);
                write_to_log_file(log_message);
                //  If client command is "quit", break. 
                if (strcmp(client_input, "quit") == 0)
                    break;
                // If client request is "killServer", updates the client queue
                // then send a SIGINT signal to the parent process, and exits the program.
                else if (strcmp(client_input, "killServer") == 0)
                {
                    printf(">> kill signal received from %s..terminating...\n", client_name);
                    client_queue->clients_arr[idx] = 0;
                    kill(getppid(), SIGINT);
                    exit(0);
                }
                // handler function for executing the right action according to the client request
                handle_commands(client_get_fifo_fd, client_send_fifo_fd, client_input);
            }
        }

        char log_message[256];
        sprintf(log_message, "%s disconnected\n", client_name);
        write_to_log_file(log_message);

        printf(">> %s disconnected..\n", client_name);

        close(client_get_fifo_fd);

        close(client_send_fifo_fd);

        client_queue->clients_arr[idx] = 0;

        sem_post(sem_client); //set sem for the client

        client_queue->client_nums--;

        exit(0);
    }
}

/**
 * For processesing different commands received from a client. It calls the appropriate command execution function. 
 * It sends the client fifo id along with the input string entered by the client
 * 
 * @param client_get_fifo_fd the file descriptor for the FIFO used by the client to receive messages from the server. It is used to read data
 * sent by the server to the client through the FIFO.
 * @param client_send_fifo_fd The `client_send_fifo_fd` parameter in the `handle_commands` function
 * represents the file descriptor for the FIFO (First In First Out) used for sending data from the
 * client to the server. This file descriptor is used to write data to the FIFO so that the server can
 * read it from its corresponding
 * @param command The `handle_commands` function takes in three parameters:
 */
void handle_commands(int client_get_fifo_fd, int client_send_fifo_fd, char *command)
{
    char **command_args = create_2d_array();

    int token_count = parse_input(command, command_args, ' ');
    char *command_str = command_args[0];
    if (strcmp(command_str, "help") == 0)
    {
        if (command_args[1] == NULL)
        {
            write(client_get_fifo_fd, "\n\nAvailable comments listed as :\n\nhelp, list, readF, writeT, upload, download, archServer, quit, killServer\n\n", BUF_SIZE);
        }
        else if (strcmp(command_args[1], "list") == 0)
        {
            write(client_get_fifo_fd, "\n\nlist\nsends a request to display the list of files in Servers directory\n\n", BUF_SIZE);
        }
        else if (strcmp(command_args[1], "readF") == 0)
        {
            write(client_get_fifo_fd, "\n\nreadF <file> <line #>\ndisplay the #th line of the <file>, returns with an error if <file> does not exists\n\n", BUF_SIZE);
        }
        else if (strcmp(command_args[1], "writeT") == 0)
        {
            write(client_get_fifo_fd, "\n\nwriteT <file> <line #> <string>\nrequest to write the content of \"string\" to the #th line the <file>, if the line # is not given writes to the end of file. If the file does not exists in Servers directory creates and edits the file at the same time\n\n", BUF_SIZE);
        }
        else if (strcmp(command_args[1], "upload") == 0)
        {
            write(client_get_fifo_fd, "\n\nupload <file>\nuploads the file from the current working directory of client to the Servers directory (beware of the cases no file in clients current working directory and file with the same name on Servers side)\n\n", BUF_SIZE);
        }
        else if (strcmp(command_args[1], "download") == 0)
        {
            write(client_get_fifo_fd, "\n\ndownload <file>\nrequest to receive <file> from Servers directory to client side\n\n", BUF_SIZE);
        }
        else if (strcmp(command_args[1], "archServer") == 0)
        {
            write(client_get_fifo_fd, "\n\narchServer <fileName>.tar\n  Archives the files that are available on the Server. Stores them in the <filename>.tar \n\n", BUF_SIZE);
        }
        else if (strcmp(command_args[1], "quit") == 0)
        {
            write(client_get_fifo_fd, "\n\nquit\nSend write request to Server side log file and quits\n\n", BUF_SIZE);
        }
        else if (strcmp(command_args[1], "killServer") == 0)
        {
            write(client_get_fifo_fd, "\n\nkillServer\nSends a kill request to the Server\n\n", BUF_SIZE);
        }
        else
        {
            write(client_get_fifo_fd, "\n\nInvalid command\n\n", BUF_SIZE);
        }
    }
    else if (strcmp(command_str, "list") == 0)
    {
        __list(client_get_fifo_fd);
    }
    else if (strcmp(command_str, "readF") == 0)
    {
        _readF(client_get_fifo_fd, command);
    }
    else if (strcmp(command_str, "writeT") == 0)
    {
        _writeT(client_get_fifo_fd, command);
    }
    else if (strcmp(command_str, "upload") == 0)
    {
        _upload(client_send_fifo_fd,client_get_fifo_fd, command);
    }
    else if (strcmp(command_str, "download") == 0)
    {
        _download(client_get_fifo_fd, command);
    }
    else if (strcmp(command_str, "archServer") == 0)
    {
        if (token_count < 2){
            write(client_get_fifo_fd, "Error: no tar filename \n", BUF_SIZE);
        }
        else{
            //send the argument that contains filename and client fifos
            _archServer(client_get_fifo_fd, command_args[1]);
        }
    }
    else
    {
        write(client_get_fifo_fd, "\n\nInvalid command\n\n", BUF_SIZE);
    }
    free_2d_array(command_args);

}
