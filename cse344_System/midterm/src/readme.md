Design and implemention of a File System Server that enables multiple clients to connect, access, modify etc. and archive the files in a specific directory.
Server and client side development is done in C. 
The Server is programmed to manage multiple clinet connections at the same time without race condisitions. Common resources in server used by clients is managed and synchronization is conducted using Semaphores.
For Inter process Communication(IPC) FIFOs used. For each new client its own FIFO created and assigned.
