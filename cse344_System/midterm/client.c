#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <semaphore.h>

#define BUFFER_SIZE 1024
#define clientSendFifo "/tmp/send_fifo_client.%d" //client uses to send info to server server will read from this fifo
#define clientGetFifo "/tmp/get_fifo_client.%d"
#define serverFifo "/tmp/neHos_server_fifo_%d"
int server_pid;
int client_fifo_fd_get;
int client_fifo_fd_send = -100;
char client_fifo_name_get[64];
char client_fifo_name_send[64];
char buffer[BUFFER_SIZE];
char input[BUFFER_SIZE];

sem_t *sem_curr_client;
char *semaphore_name;

int get_server_pid(char *argv[]);

void check_args(int argc, char *argv[]);

void check_connection_option(int argc, char *argv[]);

void handle_signal(int signum);

void create_client_fifos();

void parse_input(char *user_input, char **user_commands, char delimiter);

char *delete_spaces_from_input(char *user_input);

char **create_2d_array();

void handle_signal(int signum);

void init_signal_handler();

void init_semaphore();

void send_client_pid(int serverpid, char *connect_option);

void init_client_fifos(char *connect_option);

void handle_commands();

void handle_quit_kill_command();

void handle_readf_command(char **user_commands);
void handle_writeT_command(char **user_commands);

void handle_invalid_commands();

void handle_upload_command(char **user_commands);

void handle_download_command(char **user_commands);
void handle_archServer_command();
void handle_common();
void cleanup();

int main(int argc, char *argv[])
{

    check_args(argc, argv);

    char *connect_option = argv[1];

    server_pid = get_server_pid(argv);

    init_signal_handler();

    init_semaphore();

    // check_connection_option(argc, argv);

    create_client_fifos();

    printf(">> Waiting for Queue...");

    fflush(stdout);

    send_client_pid(server_pid, connect_option);

    init_client_fifos(connect_option);

    handle_commands();

    printf(">> Disconnected from server PID %d\n", server_pid);

    cleanup();

    return 0;
}

void parse_input(char *user_input, char **user_commands, char delimiter)
{
    char terminator = '\0';

    int i = 0;
    int k = 0;
    int z = 0;

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
            user_commands[z] = delete_spaces_from_input(user_commands[z]);

            // Increment k and z
            z++;
            k = 0;
        }

        i++;
    }

    // For the last command
    user_commands[z][k] = terminator;
    user_commands[z] = delete_spaces_from_input(user_commands[z]);

    // Make the last element of the commands array NULL
    if (user_commands[z + 1] != NULL)
    {
        free(user_commands[z + 1]);
    }
    user_commands[z + 1] = NULL;
}

char *delete_spaces_from_input(char *user_input)
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

char **create_2d_array()
{
    // create an array of strings to store the commands
    char **commands = malloc((BUFFER_SIZE) * sizeof(char *));

    // check if malloc is successful
    if (commands == NULL)
    {
        fprintf(stderr, "There is something wrong with malloc");
    }

    for (int i = 0; i < BUFFER_SIZE; i++)
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
    commands[BUFFER_SIZE] = NULL;
    return commands;
}

void check_args(int argc, char *argv[])
{
    if (argc != 3)
    {
        fprintf(stderr, "Usage: %s <connect/tryConnect> ServerPID\n", argv[0]);
        exit(1);
    }

    if (strcmp(argv[1], "connect") != 0 && strcmp(argv[1], "tryConnect") != 0)
    {
        fprintf(stderr, "Usage: %s <connect/tryConnect> ServerPID\n", argv[0]);
        exit(1);
    }
}

void cleanup()
{
    close(client_fifo_fd_get);
    close(client_fifo_fd_send);

    unlink(client_fifo_name_get);
    unlink(client_fifo_name_send);
}

// Signal Handler Functions
void handle_signal(int signum)
{
    if (client_fifo_fd_send != -100 && write(client_fifo_fd_send, "quit", BUFFER_SIZE) < 0)
    {
        perror("write");
        cleanup();
        exit(1);
    }

    // close client fifo
    close(client_fifo_fd_get);
    close(client_fifo_fd_send);

    // unlink client fifo
    unlink(client_fifo_name_get);
    unlink(client_fifo_name_send);

    exit(0);
}

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

int get_server_pid(char *argv[])
{
    int server_pid = atoi(argv[2]);
    if (server_pid < 1)
    {
        fprintf(stderr, "Invalid server PID.\n");
        return 1;
    }
    return server_pid;
}

void check_connection_option(int argc, char *argv[])
{
    if (argc != 3)
    {
        fprintf(stderr, "Usage: %s <Connect/tryConnect> ServerPID\n", argv[0]);
        exit(1);
    }
}

void create_client_fifos()
{
    // Create a FIFO for sending messages to server
    sprintf(client_fifo_name_get, clientGetFifo, getpid());
    if (mkfifo(client_fifo_name_get, 0666) < 0)
    {
        perror("mkfifo");
        exit(1);
    }

    // Create a FIFO for receiving messages from server
    sprintf(client_fifo_name_send, clientSendFifo, getpid());
    if (mkfifo(client_fifo_name_send, 0666) < 0)
    {
        perror("mkfifo");
        exit(1);
    }
}

void init_semaphore()
{
    semaphore_name = malloc(sizeof(char) * 256);
    sprintf(semaphore_name, "/semaphore_%d", server_pid);

    sem_curr_client = sem_open(semaphore_name, O_CREAT, 0644, 0);
}

/**
 * The function `send_client_pid` connects to a server FIFO, sends the client PID to the server, and
 * then closes the server FIFO.
 * 
 * @param serverpid The `serverpid` parameter in the `send_client_pid` function is an integer
 * representing the process ID of the server to which the client wants to connect.
 * @param connect_option The `connect_option` parameter in the `send_client_pid` function is a
 * pointer to a character array that specifies additional options for connecting to the server. It is
 * used to customize the connection behavior or provide specific instructions for the server
 * communication. You can pass different values or strings as `connect_option`
 */
void send_client_pid(int serverpid, char *connect_option)
{
    char server_fifo_name[128];

    sprintf(server_fifo_name, serverFifo, server_pid);

    // Connect to server fifo for communication
    int server_fifo_fd = open(server_fifo_name, O_WRONLY);
    if (server_fifo_fd < 0)
    {
        perror("open server fifo");
        cleanup();
        exit(1);
    }

    int client_pid = getpid();

    char client_pid_str[128];

    sprintf(client_pid_str, "%d%s", client_pid, connect_option);

    // Send client PID to server
    if (write(server_fifo_fd, client_pid_str, 128) < 0)
    {
        perror("write");
        cleanup();
        exit(1);
    }

    // Close server FIFO
    close(server_fifo_fd);

    sem_post(sem_curr_client);
}

void init_client_fifos(char *connect_option)
{
    // Open client FIFO for reading from server
    
    client_fifo_fd_get = open(client_fifo_name_get, O_RDONLY);
    if (client_fifo_fd_get < 0)
    {
        perror("open client get");
        cleanup();
        exit(1);
    }

    if (strcmp(connect_option, "tryConnect") == 0)
    {
        if (read(client_fifo_fd_get, buffer, BUFFER_SIZE) < 0)
        {
            perror("read");
            cleanup();
            exit(1);
        }
        //if no place available in the queue 
        if (strcmp(buffer, "full") == 0){
            cleanup();
            exit(1);
        }
    }

    // Open server FIFO
    client_fifo_fd_send = open(client_fifo_name_send, O_WRONLY);
    if (client_fifo_fd_send < 0)
    {
        perror("open client send");
        cleanup();
        exit(1);
    }

    printf("Connection established:\n");
}
void handle_commands()
{
    while (1)
    {
        printf(">> Enter comment: ");

        fgets(input, BUFFER_SIZE, stdin);

        input[strcspn(input, "\n")] = '\0';

        char **user_commands = create_2d_array();

        parse_input(input, user_commands, ' ');

        if (strcmp(user_commands[0], "help") == 0)
        {
            handle_common();
        }
        else if(strcmp(user_commands[0],"archServer") == 0){
            handle_archServer_command();
        }
        else if (strcmp(user_commands[0], "killServer") == 0)
        {
            handle_quit_kill_command();
        }
        else if (strcmp(user_commands[0], "quit") == 0)
        {
            handle_quit_kill_command();
        }
        else if (strcmp(user_commands[0], "list") == 0)
        {
            handle_common();
        }
        else if (strcmp(user_commands[0], "readF") == 0)
        {
            handle_readf_command(user_commands);
        }
        else if (strcmp(user_commands[0], "upload") == 0)
        {
            handle_upload_command(user_commands);
        }
        else if (strcmp(user_commands[0], "download") == 0)
        {
            handle_download_command(user_commands);
        }
        else if (strcmp(user_commands[0], "writeT") == 0)
        {
            handle_writeT_command(user_commands);
        }
        
        else
        {
            handle_invalid_commands();
        }
        // memset(buffer, 0, sizeof(buffer));

    }
}
void handle_common()
{
    //send client input to server
    if (write(client_fifo_fd_send, input, BUFFER_SIZE) < 0)
    {
        printf("write error\n");
        perror("write");
        cleanup();
        exit(1);
    }

    // read from get fifo which contain the server resposnse 
    if (read(client_fifo_fd_get, buffer, BUFFER_SIZE) < 0)
    {
        perror("read");
        cleanup();
        exit(1);
    }

    printf("%s\n", buffer);
    memset(buffer, 0, sizeof(buffer));


}
void handle_archServer_command(){
    //send client input to server
    if (write(client_fifo_fd_send, input, BUFFER_SIZE) < 0)
    {
        printf("write error\n");
        perror("write");
        cleanup();
        exit(1);
    }

    // read from get fifo which contain the server resposnse 
    // if (read(client_fifo_fd_get, buffer, BUFFER_SIZE) < 0)
    // {
    //     perror("read");
    //     cleanup();
    //     exit(1);
    // }

    // printf("%s\n", buffer);
    // memset(buffer, 0, sizeof(buffer));
    int bytes= 0 ;
    while ((bytes = read(client_fifo_fd_get, buffer, BUFFER_SIZE)) > 0)
    {
        if(bytes<0){
            perror("read");
            cleanup();
            exit(1);
        }
        if (strcmp(buffer, "Y") == 0)
        {
            break;
        }
        else if(strcmp(buffer, "N") == 0){
            printf("%s\n", buffer);
            break;
        }

        printf("%s\n", buffer);
    }
    printf("\n");
    memset(buffer, 0, sizeof(buffer));
}

void handle_writeT_command(char **user_commands)
{
    if (user_commands[1] == NULL)
    {
        printf("\nInvalid write command\n\n");
        return;
    }

    // send to server fifo
    if (write(client_fifo_fd_send, input, BUFFER_SIZE) < 0)
    {
        perror("write");
        cleanup();
        exit(1);
    }

    if (read(client_fifo_fd_get, buffer, BUFFER_SIZE) < 0)
    {
        perror("read");
        cleanup();
        exit(1);
    }

    //check if server response is success
    if (strcmp(buffer, "Y") != 0)
    {
        printf("writeT Error: %s\n", buffer);
        memset(buffer, 0, sizeof(buffer));
        return;
    }
    printf("Write succeeded\n");
    memset(buffer, 0, sizeof(buffer));
    printf("\n");

}
void handle_readf_command(char **user_commands)
{
    if (user_commands[1] == NULL)
    {
        printf("\nInvalid read command\n\n");
        return;
    }

    // send to server fifo
    if (write(client_fifo_fd_send, input, BUFFER_SIZE) < 0)
    {
        perror("write");
        cleanup();
        exit(1);
    }

    if (read(client_fifo_fd_get, buffer, BUFFER_SIZE) < 0)
    {
        perror("read");
        cleanup();
        exit(1);
    }

    if (strcmp(buffer, "Y") != 0)
    {
        printf("Error: %s\n",buffer);
        memset(buffer, 0, sizeof(buffer));

        return;
    }
    int bytes_read = 0;

    while ((bytes_read = read(client_fifo_fd_get, buffer, BUFFER_SIZE)) > 0)
    {
        if (strcmp(buffer, "XX") == 0)
        {
            break;
        }
        else if(strcmp(buffer, "N") == 0){
            printf("%s\n", buffer);
            break;
        }

        printf("%s\n", buffer);
    }
    printf("\n");
    memset(buffer, 0, sizeof(buffer));

}

void handle_quit_kill_command()
{
    // send to server fifo
    if (write(client_fifo_fd_send, input, BUFFER_SIZE) < 0)
    {
        perror("write");
        cleanup();
        exit(1);
    }
    exit(0);
}


void handle_invalid_commands()
{
    // send to server fifo
    if (write(client_fifo_fd_send, input, BUFFER_SIZE) < 0)
    {
        perror("write");
        cleanup();
        exit(1);
    }

    // read from get fifo
    if (read(client_fifo_fd_get, buffer, BUFFER_SIZE) < 0)
    {
        perror("read");
        cleanup();
        exit(1);
    }

    printf("%s\n", buffer);
}


void handle_upload_command(char **user_commands)
{
    if (user_commands[1] == NULL)
    {
        printf("Error: No filename specified\n");
        return;
    }
    char *filename = user_commands[1];

    int file_fd = open(filename, O_RDONLY);
    if (file_fd == -1)
    {
        printf("Error: File does not exist\n");
        return;
    }

    if (write(client_fifo_fd_send, input, sizeof(input)) < 0)
    {
        perror("write");
        cleanup();
        exit(1);
    }

    printf("File transfer request received. Beginning file transfer:\n");

    char buffer[1024];
    ssize_t total_bytes_read = 0;
    ssize_t bytes_read;

    while ((bytes_read = read(file_fd, buffer, BUFFER_SIZE)) > 0)
    {
        total_bytes_read += bytes_read;
        write(client_fifo_fd_send, buffer, bytes_read); // Write the read data to the server FIFO
    }

    // send something uniuqe to indicate end of file
    write(client_fifo_fd_send, "XX", 2);

    printf("%ld bytes transferred\n\n", total_bytes_read);

    close(file_fd);
    memset(buffer, 0, sizeof(buffer));
}

void handle_download_command(char **user_commands)
{

    if (user_commands[1] == NULL)
    {
        printf("Error: No filename specified\n");
        return;
    }

    if (write(client_fifo_fd_send, input, BUFFER_SIZE) < 0)
    {
        perror("write");
        cleanup();
        exit(1);
    }
//read server response
    char res;
    if (read(client_fifo_fd_get, &res, 1) < 0)
    {
        perror("read");
        cleanup();
        exit(1);
    }

    if (res == 'N')
    {
        printf("Error: File does not exists\n");
        return;
    }

    int file_fd = open(user_commands[1], O_WRONLY | O_CREAT | O_TRUNC, 0666); // Open or create the file for writing
    if (file_fd == -1)
    {
        char error_message[BUFFER_SIZE];
        printf("File could not be opened for writing\n");
        return;
    }

    char buffer[BUFFER_SIZE];
    ssize_t bytes_read;
    ssize_t total_bytes_read = 0;
    //read file content incoming from server
    while ((bytes_read = read(client_fifo_fd_get, buffer, sizeof(buffer))) > 0)
    {
        if (buffer[bytes_read - 1] == 'X' && buffer[bytes_read - 2] == 'X')
        {
            buffer[bytes_read - 2] = '\0';
            write(file_fd, buffer, bytes_read - 2);
            memset(buffer, 0, sizeof(buffer));

            break;
        }
        total_bytes_read+=bytes_read;

        write(file_fd, buffer, bytes_read);
    }
    
    close(file_fd);
    printf("Download Success %ld bytes stored\n\n", total_bytes_read);
    memset(buffer, 0, sizeof(buffer));

}



