#include "include/fat12.h"

using namespace filesystem12;

int main (int argc, char** argv) 
{
    // sample usage: ./fileSystemOper mysystem.dat <command> <command args>
    // check if arguments are valid
    if (argc > 6 || argc < 3)
    {
        std::cerr << "Usage: ./fileSystemOper <file name> <command> <command args>" << std::endl;
        return 1;
    } 
    std::string pass = "";
    //check if password argument provided. If password exist operate by cheching password match 
    if(argc == 6){
        pass=argv[argc-1];
    }
    // create a fat12 object
    fatTable *fat12 = new fatTable();

    // start the fat12 object
    if (!fat12->start(argv[1]))
    {
        delete fat12;
        return 1;
    }
    
    //if system started, control block for the user command
    else if (strcmp(argv[2], "dir") == 0)
    {
        if (!(argc == 4 || argc == 5))
        {
            std::cerr << "Usage: ./fileSystemOper <file name> dir <filepath>" << std::endl;
            delete fat12;
            return 1;
        }
        if (argc == 5){      
            fat12->f_listDir(argv[3], argv[4]);
        }
        else{
            fat12->f_listDir(argv[3], pass);
        }
    }

    // mkdir
    else if (strcmp(argv[2], "mkdir") == 0)
    {
        if (!(argc == 4 || argc == 5))
        {
            std::cerr << "Usage: ./fileSystemOper <file name> mkdir <filepath>" << std::endl;
            delete fat12;
            return 1;
        }
        if (argc == 5){      
            fat12->f_mkDir(argv[3], argv[4]);
        }
        else{
            fat12->f_mkDir(argv[3], pass);
        }
    }

    
    else if (strcmp(argv[2], "rmdir") == 0)
    {
        if (!(argc == 4 || argc == 5))
        {
            std::cerr << "Usage: ./fileSystemOper <file system> rmdir <filepath>" << std::endl;
            delete fat12;
            return 1;
        }
        if (argc == 5){      
            fat12->f_rmDir(argv[3], argv[4]);
        }
        else{
            fat12->f_rmDir(argv[3], pass);
        }

    }
    else if (strcmp(argv[2], "dumpe2fs") == 0)
    {
        fat12->f_dumpe2fs();
    }
    // write
    else if (strcmp(argv[2], "write") == 0)
    {
        if (!(argc == 5 || argc == 6))
        {
            std::cerr << "Usage: ./fileSystemOper <file system> write <filepath> <fileToRead>" << std::endl;
            delete fat12;
            return 1;
        }
        if (argc == 6){      
            fat12->f_write(argv[3], argv[4], argv[5]);
        }
        else{
            fat12->f_write(argv[3], argv[4], pass);
        }
            
    }

    // read
    else if (strcmp(argv[2], "read") == 0)
    {
        if (!(argc == 5 || argc == 6))
        {
            std::cerr << "Usage: ./fileSystemOper <file system> read <filepath> <fileToWrite>" << std::endl;
            delete fat12;
            return 1;
        }
        if (argc == 6){      
            fat12->f_read(argv[3], argv[4], argv[5]);
        }
        else{
            fat12->f_read(argv[3], argv[4], pass);
        }
    }

    // del
    else if (strcmp(argv[2], "del") == 0)
    {
        if (argc < 4)
        {
            std::cerr << "Usage: ./fileSystemOper <file system> del <filepath>" << std::endl;
            delete fat12;
            return 1;
        }
        if (argc == 6){      
            fat12->f_del(argv[3], argv[4]);
        }
        else{
            fat12->f_del(argv[3],  pass);
        }

    }
    else if (strcmp(argv[2], "addpw") == 0)
    {
        if (argc != 5)
        {
            std::cerr << "Usage: ./fileSystemOper <file system> addpw <filepath> usr_password" << std::endl;
            delete fat12;
            return 1;
        }

        if (!fat12->f_addpw(argv[3], argv[4])) {
            std::cerr << "Failed to add password" << std::endl;
        }
    }

    else if (strcmp(argv[2], "chmod") == 0)
    {
        if (argc != 5)
        {
            std::cerr << "Usage: ./fileSystemOper <file system> chmod <path> permissions" << std::endl;
            delete fat12;
            return 1;
        }

        fat12->f_chmod(argv[3], argv[4]);
    }

    else 
    {
        std::cerr << "Error: Invalid command" << std::endl;
        delete fat12;
        return 1;
    }
    
    delete fat12;

    return 0;
}