
#ifndef FAT12_H_
#define FAT12_H_

#include <cstdlib>
#include <cstdio>    
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <ctime>
#include <cstring>
#include <sstream>
#include <cerrno>

#define _KB_ 1024
#define _MB_ 1024*_KB_
#define MAGIC_NUMBER 15
bool stringToSizeT (std::string str, size_t* val);
namespace filesystem12
{
    const int READ_PERM = 444;
    const int WRITE_PERM = 222;
    const int EXECUTE_PERM= 111;
    

    //Initialize data structures to be used in the file system
    //superblock struct: metadata about the file system.

    struct superblock
    {
        int magicNumber; //Identifier for the file system type.
        size_t rootP; //position of root dir
        size_t blockSize; //size of each block in the file system.
        size_t numOfBlocks; //tot number of blocks in the file system.
        size_t blockCount; //currently used number of blocks used.
        size_t freeCount;
        size_t fileCount;
        size_t dirCount;
    };
    //for a single block in the file system.
    struct block
    {
        size_t blockNum;
        ssize_t nextBlock;
        bool isDir;
        std::vector<char> data;
    };
    //to represent the FAT12 table.
    struct fat12_b
    {
        superblock sb; //superblock obj 
        std::vector<block> blocks; // to store all the block in the system
        std::vector<block> freeBlocks;// list of free blocks in the system
    };
    //to represnt a file or a directory entry
    struct directoryEntry
    {
        std::string fileName;
        bool isDir; 
        size_t file_size; // size of the file
        std::string extension; 
        std::string time;
        std::string date;
        size_t blockNum;//block number indicating where the file or dir starts 
        int permissions = 555; // perms of the dir
        std::string password; // attr. for password

    };
    struct directory
    {
        std::string dirPath;
        size_t entryNum;
        size_t blockNum;
        int permissions = 555;
        std::string password;  

        std::vector<directoryEntry> entries;
    };

    struct file
    {
        std::string fileName;
        std::string fileExtension;
        std::string fileTime;
        std::string fileDate;
        size_t fileLength;
        size_t blockNum;
        int permissions = 555; //read &execute perms
        std::string password;  

    };


    //FAT12 file system 
    class fatTable
    {

    private:
        fat12_b fat12;
        bool isInitialized;
        std::string fileSystemName;

        
        bool write_f_data (const block& fileBlock, std::string fileNameToWrite, size_t fileLength);

        bool srl_dir (const directory& d, std::vector<char>& data);
        bool desrl_dir (directory& d, const block& directoryBlock);
        bool srl_file (file &f, const std::string& fileNameToRead, std::vector<char>& data);
        bool desrl_file (file& f, const block& fileBlock);


        bool updt_dir (directory& d);
        // starts from root till the wanted directory
        bool search_dir (directory d, std::string path, directory& result, const std::string& directoryName);

        bool search_block (size_t blockNumber, block& b);
        bool init_block (size_t blockNumber, const block& b);
        bool rm_file (block& startBlock);
        bool rm_block (block& b);
        
    protected:
        bool write_fat12(fat12_b &fat12, std::ofstream &file);
        bool read_fat12 (fat12_b& fat12, std::ifstream& file);


        // directory functions
        bool show_dir(const directory &d);
        bool show_dirEntry(const directoryEntry &d);

        // binary block writers
        bool write_sb(superblock &sb, std::ofstream &file);
        bool write_block(block &b, std::ofstream &file);

        // binary block readers
        bool read_sb(superblock &sb, std::ifstream &file);
        bool read_block(block &b, std::ifstream &file);

        bool set_modified(std::string &time, std::string &date);
        

    public:
        
        fatTable ();
        ~fatTable ();
        // initalize the file system, blocksize can be 0.5, 1, 2, 4
        bool init (std::string blockSize);

        // start the file system, with reading block size from binary file
        bool start (std::string fileName);
        // read from binary file
        bool readFile (std::string fileName);
       
        // write to binary file
        bool writeFile (std::string fileName);
        // list the contents of the directory
        bool f_listDir (std::string path, std::string pass);

        // create a new directory
        bool f_mkDir (std::string path, std::string pass);

        // remove a directory
        bool f_rmDir (std::string path, std::string pass);

        // give information about file system
        bool f_dumpe2fs ();

        // creates and writes data to the file
        bool f_write (std::string path, std::string fileNameToReadContent, std::string pass);
    
        // reads data from the file
        bool f_read (std::string path, std::string fileNameToWrite, std::string pass);

        // removes a file
        bool f_del (std::string path, std::string pass);
        bool f_chmod(std::string path, std::string permissions);
        bool f_addpw(std::string path, std::string password);

    };
}
#endif 