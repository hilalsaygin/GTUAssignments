#include "../include/fat12.h"
using namespace std;
using namespace filesystem12;
// global function for conversion block numbers to size_t type
bool stringToSizeT(std::string str, size_t* val)
{
    *val = 0;

    for (size_t i = 0; i < str.length(); i++)
    {
        if (str[i] < '0' || str[i] > '9')
        {
            return false;
        }

        *val *= 10;
        *val += str[i] - '0';
    }

    return true;
}
// writes the current state of the FAT12 system into given fileName.
bool fatTable::writeFile(string fileName)
{
    if (!isInitialized)
        return false;

    ofstream file;
    // open file in writing mode
    file.open(fileName, ios::out | ios::binary);

    fileSystemName = fileName;

    if (!file.is_open())
        return false;

    return write_fat12(fat12, file);
}
// writes the entire fat12 file system structure into binary file
bool fatTable::write_fat12(fat12_b &fat12, ofstream &file)
{
    // first write the superblock of the system into the file
    if (!write_sb(fat12.sb, file))
        return false;

    // then write the rest of the system blocks
    for (size_t i = 0; i < fat12.blocks.size(); i++)
    {
        if (!write_block(fat12.blocks[i], file))
            return false;
    }

    // write the free blocks in the system fat12
    for (size_t i = 0; i < fat12.freeBlocks.size(); i++)
    {
        if (!write_block(fat12.freeBlocks[i], file))
            return false;
    }
    // all fat12 system stored into the given file paremeter, return true
    return true;
}

bool fatTable::write_block(block &b, ofstream &file)
{
    // write block struct to binary file

    if (!file.is_open())
        return false;

    // write block number
    file.write((char *)&b.blockNum, sizeof(b.blockNum));

    // write next block number
    file.write((char *)&b.nextBlock, sizeof(b.nextBlock));

    // write is directory it is a boolean type
    file.write((char *)&b.isDir, sizeof(b.isDir));

    // write data
    for (size_t i = 0; i < b.data.size(); i++)
    {
        file.write((char *)&b.data[i], sizeof(b.data[i]));
    }

    return true;
}
bool fatTable::readFile(string fileName)
{
    ifstream file;
    // openfile in reading mode
    file.open(fileName, ios::in | ios::binary);

    if (!file.is_open())
        return false;

    return read_fat12(fat12, file);
}


bool fatTable::read_fat12(fat12_b &fat12, ifstream &file)
{
    superblock sb;
    // first read the superblock from the file into temp sb object
    if (!read_sb(sb, file))
        return false;
    // set superblock attribute of the system
    fat12.sb = sb;

    // allocate new blocks for the fat12 system
    for (size_t i = 0; i < fat12.sb.blockCount; i++)
    {
        // create temp block object t
        block b;
        b.blockNum = i;
        b.isDir = false;
        b.nextBlock = -1;
        // initialize data of block object as zero
        for (size_t j = 0; j < fat12.sb.blockSize; j++)
        {
            b.data.push_back('0');
        }
        // push the block object into file system
        fat12.blocks.push_back(b);
    }

    // allocate free blocks
    for (size_t i = 0; i < fat12.sb.freeCount; i++)
    {
        block b;
        b.blockNum = i;
        b.isDir = false;
        b.nextBlock = -1;
        // initialize data
        for (size_t j = 0; j < fat12.sb.blockSize; j++)
        {
            b.data.push_back('0');
        }

        fat12.freeBlocks.push_back(b);
    }

    // read blocks
    for (size_t i = 0; i < fat12.blocks.size(); i++)
    {
        if (!read_block(fat12.blocks[i], file))
            return false;
    }

    // read free blocks
    for (size_t i = 0; i < fat12.freeBlocks.size(); i++)
    {
        if (!read_block(fat12.freeBlocks[i], file))
            return false;
    }

    return true;
}



bool fatTable::read_block(block &b, ifstream &file)
{
    // read block struct from binary file

    if (!file.is_open())
        return false;

    // read block number
    file.read((char *)&b.blockNum, sizeof(b.blockNum));

    // read next block number
    file.read((char *)&b.nextBlock, sizeof(b.nextBlock));

    // read is directory it is a boolean type
    file.read((char *)&b.isDir, sizeof(b.isDir));

    // read data
    for (size_t i = 0; i < b.data.size(); i++)
    {
        file.read((char *)&b.data[i], sizeof(b.data[i]));
    }

    return true;
}

bool fatTable::write_sb(superblock &sb, ofstream &file)
{
    // write superblock struct to binary file

    if (!file.is_open())
        return false;

    file.write((char *)&sb, sizeof(superblock));
    return true;
}

bool fatTable::read_sb(superblock &sb, ifstream &file)
{
    // read superblock struct from binary file

    if (!file.is_open())
        return false;

    file.read((char *)&sb, sizeof(superblock));
    return true;
}

fatTable::fatTable()
{
    isInitialized = false;
}

fatTable::~fatTable()
{
}

bool fatTable::init(string blockSize)
{
    // check if already initialized
    if (isInitialized)
        return false;

    // check block size. The file system only supports blck size of 0.5kb and 1kb
    if (blockSize != "0.5" && blockSize != "1")
    {
        printf("Invalid block size!! 0.5KB and 1KB supported");
        return false;
    }

    size_t mb = _MB_;
    size_t size_of_partition = 0;
    size_t size_of_block = 0;

    if (blockSize == "0.5")
    {
        size_of_partition = mb * 2;
        size_of_block = _KB_ / 2;
    }
    else if (blockSize == "1")
    {
        size_of_partition = mb * 4;
        size_of_block = _KB_;
    }

    // initialize the superblock attributes
    fat12.sb.blockSize = size_of_block;
    fat12.sb.numOfBlocks = size_of_partition / size_of_block;
    fat12.sb.blockCount = 1; // root directory itself
    fat12.sb.magicNumber = MAGIC_NUMBER;
    fat12.sb.rootP = 0;
    fat12.sb.freeCount = 0;
    fat12.sb.fileCount = 0;
    fat12.sb.dirCount = 1; // root directory

    // create free blocks, except the root dir. mark them as free
    for (size_t i = 1; i < fat12.sb.numOfBlocks; i++)
    {
        block b;
        b.blockNum = i;
        b.isDir = false;
        b.nextBlock = -1;
        // initialize data in blocks as 0
        for (size_t j = 0; j < fat12.sb.blockSize; j++)
        {
            b.data.push_back('0');
        }
        // add the new block inot the free blocks list of the fat12 object
        fat12.freeBlocks.push_back(b);
        fat12.sb.freeCount++;
    }

    // initialize the root directory block
    block rblock;
    rblock.blockNum = 0;
    rblock.nextBlock = -1;
    rblock.isDir = true;
    // initialize data
    for (size_t j = 0; j < fat12.sb.blockSize; j++)
    {
        rblock.data.push_back('0');
    }
    // create directory object for root
    vector<char> rootDirData; // vector initialize to hold root data

    directory rootDirectory;
    rootDirectory.dirPath = "\\"; // root path
    rootDirectory.entryNum = 0;
    rootDirectory.blockNum = 0;
    rootDirectory.entries = vector<directoryEntry>(); // create an empty vector to hold entries

    srl_dir(rootDirectory, rootDirData);

    // set root block data same as directory data
    rblock.data = rootDirData;
    fat12.blocks.push_back(rblock); // add to blocks list

    isInitialized = true; // mark as initalized
    return true;
}
// reads from the provided filename. Then sets the file system name as the parameter fileName
// marks as initialized, and returns true
bool fatTable::start(string fileName)
{
    // return false, if already initialized
    if (isInitialized)
        return false;

    // read the fat12 object
    if (!readFile(fileName))
    {
        // cerr << "File name not found" << endl;
        perror("Start");
        return false;
    }

    fileSystemName = fileName;

    isInitialized = true;
    return true;
}

// search directory starting from d, last ..\dirname is the directory to be searched
bool fatTable::search_dir(directory d, string path, directory &result, const string &directoryName)
{
    // check if path does not contain any \ symbol
    if (path == "\\" || path.empty())
    {
        // set result to d
        result = d;
        return true;
    }

    // discard root directory from path
    if (path[0] == '\\')
        path = path.substr(1);

    // get next directory name
    string nextDirectoryName = path.substr(0, path.find_first_of("\\"));

    // discard next directory name from path
    if (path.find_first_of("\\") != string::npos)
        path = path.substr(path.find_first_of("\\") + 1);
    else
        path = "";

    // search for next directory name in current directory
    bool found = false;
    for (size_t i = 0; i < d.entryNum; ++i)
    {
        if (d.entries[i].fileName == nextDirectoryName)
        {
            // found
            found = true;

            // check if it is a directory
            if (!d.entries[i].isDir)
            {
                cerr << "Error: " << nextDirectoryName << " is not a directory" << endl;
                return false;
            }

            // get next directory
            block nextDirectoryBlock;
            if (!search_block(d.entries[i].blockNum, nextDirectoryBlock))
            {
                cerr << "Error: Could not find next directory block" << endl;
                return false;
            }

            // deserialize next directory
            directory nextDirectory;
            if (!desrl_dir(nextDirectory, nextDirectoryBlock))
            {
                cerr << "Error: Could not deserialize next directory" << endl;
                return false;
            }

            // call with next directory
            return search_dir(nextDirectory, path, result, directoryName);
            break;
        }
    }

    if (!found)
    {
        cerr << "Error: Directory not found" << endl;
        return false;
    }

    // recursive call
    return search_dir(d, path, result, directoryName);
}
bool fatTable::show_dir(const directory &d)
{
    cout << "\nDirPath: " << d.dirPath << endl;

    cout << "Type\tModified\t\tSize\tName\t\tPerms" << endl;
    cout << "----\t-------------\t\t-----\t-----\t\t-----" << endl;

    for (size_t i = 0; i < d.entryNum; ++i)
    {
        cout << (d.entries[i].isDir ? "dr" : "fl") << "\t" << d.entries[i].date << " " << d.entries[i].time << "\t" << d.entries[i].file_size << "\t" << d.entries[i].fileName << "\t\t" << d.entries[i].permissions;
        if (!d.entries[i].isDir && d.entries[i].extension != "0")
            cout << "." << d.entries[i].extension;
        cout << endl;
    }
    return true;
}

bool fatTable::set_modified(string &time, string &date)
{
    // Get the current system time
    time_t currentTime = std::time(nullptr);
    tm *localTime = localtime(&currentTime);

    // Format the time as HH:MM
    char timeBuffer[6];
    strftime(timeBuffer, sizeof(timeBuffer), "%H:%M", localTime);
    time = timeBuffer;

    // Format the date as DD.MM.YYYY
    char dateBuffer[11];
    strftime(dateBuffer, sizeof(dateBuffer), "%d.%m.%Y", localTime);
    date = dateBuffer;

    return true;
}

bool fatTable::show_dirEntry(const directoryEntry &d)
{

    cout << "Type\t\tModified\t\tSize\tName\t\tExt" << endl;
    cout << "----\t\t-------------\t\t-----\t-----\t\t-----" << endl;

    cout << (d.isDir ? "dr" : "fl") << "\t\t" << d.date << " " << d.time << "\t\t" << d.file_size << "\t" << d.fileName << "." << d.extension << endl;

    return true;
}

bool fatTable::updt_dir(directory &d)
{
    // update directory in blocks

    // get directory block
    block directoryBlock;
    if (!search_block(d.blockNum, directoryBlock))
    {
        cerr << "Error: Could not find directory block" << endl;
        return false;
    }

    // serialize directory
    vector<char> directoryData;
    if (!srl_dir(d, directoryData))
    {
        cerr << "Error: Could not serialize directory" << endl;
        return false;
    }

    // if directory data fits in the same block
    if (directoryData.size() <= fat12.sb.blockSize)
    {
        // set data
        directoryBlock.data = directoryData;

        // update directory block
        if (!init_block(d.blockNum, directoryBlock))
        {
            cerr << "Error: Could not update directory block" << endl;
            return false;
        }
    }
    else
    {
        // add directory data to current block as much as possible
        for (size_t i = 0; i < fat12.sb.blockSize; i++)
        {
            directoryBlock.data[i] = directoryData[i];
        }

        // discard directory data that is already written
        directoryData.erase(directoryData.begin(), directoryData.begin() + fat12.sb.blockSize);

        // add directory block to blocks
        fat12.blocks.push_back(directoryBlock);

        // increase block count
        fat12.sb.blockCount++;

        size_t remainingBytes = directoryData.size() - fat12.sb.blockSize;
        bool done = false;
        while (true)
        {
            if (fat12.freeBlocks.size() == 0)
            {
                cerr << "Error: File system is full" << endl;
                return false;
            }

            // get next block from free list
            block nextBlock = fat12.freeBlocks.back();

            // remove last block from free list
            fat12.freeBlocks.pop_back();

            // decrease free block count
            fat12.sb.freeCount--;

            // set next block to -1
            nextBlock.nextBlock = -1;

            // set next block of current block
            directoryBlock.nextBlock = nextBlock.blockNum;

            // set directory block to blocks
            if (!init_block(directoryBlock.blockNum, directoryBlock))
            {
                cerr << "Error: Could not set directory block" << endl;
                return false;
            }

            // set directory block number
            directoryBlock = nextBlock;

            // add directory data to current block as much as possible
            for (size_t i = 0; i < fat12.sb.blockSize; i++)
            {
                directoryBlock.data[i] = directoryData[i];
            }

            // discard directory data that is already written
            if (!done)
                directoryData.erase(directoryData.begin(), directoryData.begin() + fat12.sb.blockSize);

            // add next block to blocks
            fat12.blocks.push_back(directoryBlock);

            // increase block count
            fat12.sb.blockCount++;

            if (done)
                break;

            // consider underflow
            if (remainingBytes <= fat12.sb.blockSize)
            {
                done = true;
                continue;
            }

            remainingBytes -= fat12.sb.blockSize;
        }
    }
    return true;
}

bool fatTable::search_block(size_t blockNum, block &b)
{
    // find block with block number
    for (size_t i = 0; i < fat12.blocks.size(); ++i)
    {
        if (fat12.blocks[i].blockNum == blockNum)
        {
            b = fat12.blocks[i];
            return true;
        }
    }

    return false;
}

bool fatTable::init_block(size_t blockNum, const block &b)
{
    // set block with block number
    for (size_t i = 0; i < fat12.blocks.size(); ++i)
    {
        if (fat12.blocks[i].blockNum == blockNum)
        {
            fat12.blocks[i] = b;
            fat12.blocks[i].data = b.data;
            return true;
        }
    }

    return false;
}

bool fatTable::desrl_file(file &f, const block &fileBlock)
{
    // deserialize file struct from block

    // get data from block
    vector<char> data = fileBlock.data;

    // convert data to string
    string content(data.begin(), data.end());

    block tempBlock = fileBlock;

    // complete the content
    while (tempBlock.nextBlock != -1)
    {
        // get next block
        block nextBlock;

        if (!search_block(tempBlock.nextBlock, nextBlock))
        {
            cerr << "Error: Could not find next block" << endl;
            return false;
        }

        // get data from next block
        vector<char> data = nextBlock.data;

        // convert data to string
        string content2(data.begin(), data.end());

        // append content2 to content
        content += content2;

        tempBlock = nextBlock;
    }

    // parse content
    istringstream iss(content);

    // file name
    getline(iss, f.fileName);

    // file extension
    getline(iss, f.fileExtension);

    // file time
    getline(iss, f.fileTime);

    // file date
    getline(iss, f.fileDate);

    // file length
    string fileLength;

    getline(iss, fileLength);

    // convert string to size_t
    size_t fileLengthSizeT = 0;
    if (!stringToSizeT(fileLength, &fileLengthSizeT))
        return false;

    f.fileLength = fileLengthSizeT;

    // block number
    string blockNum;

    getline(iss, blockNum);

    // convert string to size_t
    size_t blockNumSizeT = 0;
    if (!stringToSizeT(blockNum, &blockNumSizeT))
        return false;

    f.blockNum = blockNumSizeT;

    //permissions
    string permissions;
    getline(iss, permissions);
    f.permissions = stoi(permissions);
    // password
    getline(iss, f.password);
    return true;
}

bool fatTable::srl_file(file &f, const string &filename_, vector<char> &data)
{
    // serialize file struct to vector of chars

    string content = "";
    
    // set to file length size from filename_
    // open file
    ifstream file(filename_, ios::binary);

    if (!file.is_open())
    {
        cerr << "Error: Could not open file: " << filename_ << endl;
        return false;
    }

    // get file length
    file.seekg(0, ios::end);
    size_t fileLength = file.tellg();
    file.seekg(0, ios::beg);
    f.fileLength = fileLength;

    // read file content
    vector<char> fileContent(fileLength);
    file.read(fileContent.data(), fileLength);

    // convert file content to string
    string fileContentString(fileContent.begin(), fileContent.end());

    // append file content to content
    content += fileContentString;
    content += f.fileName + "\n";
    content += f.fileExtension + "\n";
    content += f.fileTime + "\n";
    content += f.fileDate + "\n";
    content += to_string(f.fileLength) + "\n";
    content += to_string(f.blockNum) + "\n";
    content += to_string(f.permissions) + "\n";
    content += f.password + "\n";  // Serialize password

    // convert content to vector of chars
    for (size_t i = 0; i < content.size(); i++)
    {
        data.push_back(content[i]);
    }

    // fill the rest of the block with zeros
    if (content.size() % fat12.sb.blockSize != 0)
    {
        for (size_t i = content.size() % fat12.sb.blockSize; i < fat12.sb.blockSize; i++)
        {
            data.push_back('0');
        }
    }
    content += to_string(f.permissions)+"\n";
    // Serialize password
    content += f.password + "\n";

    return true;
}
bool fatTable::srl_dir(const directory &d, vector<char> &data)
{
    // serialize directory struct to vector of chars

    string content = "";

    // directory path
    content += d.dirPath + "\n";

    // number of entries
    content += to_string(d.entryNum) + "\n";

    // block number
    content += to_string(d.blockNum)+ "\n";

    // entries
    for (size_t i = 0; i < d.entries.size(); ++i)
    {
        // file name
        content += d.entries[i].fileName+ "\n";

        // is directory
        content += to_string(d.entries[i].isDir) + "\n";

        // length
        content += to_string(d.entries[i].file_size) + "\n";

        // extension
        content += d.entries[i].extension + "\n";

        // time
        content += d.entries[i].time + "\n";

        // date
        content += d.entries[i].date+ "\n";

        // block number
        content += to_string(d.entries[i].blockNum) + "\n";
        content += to_string(d.entries[i].permissions) + "\n";
        content += d.entries[i].password + "\n";
    }

    data.clear();

    // convert content to vector of chars
    for (size_t i = 0; i < content.size(); i++)
    {
        data.push_back(content[i]);
    }

    // fill the rest of the block with zeros
    for (size_t i = content.size(); i < fat12.sb.blockSize; i++)
    {
        data.push_back('0');
    }

    return true;
}

bool fatTable::desrl_dir(directory &d, const block &directoryBlock)
{
    // deserialize directory struct from block

    // get data from block
    vector<char> data = directoryBlock.data;

    // convert data to string
    string content(data.begin(), data.end());

    block tempBlock = directoryBlock;

    // complete the content
    while (tempBlock.nextBlock != -1)
    {
        // get next block
        block nextBlock;

        if (!search_block(tempBlock.nextBlock, nextBlock)){
            cerr << "Error: Could not find next block" << endl;
            return false;
        }

        // get data from next block
        data = nextBlock.data;
        // convert data to string
        string content2(data.begin(), data.end());

        // append content2 to content
        content += content2;

        tempBlock = nextBlock;
    }

    // parse content
    istringstream iss(content);

    // directory path
    getline(iss, d.dirPath);

    // number of entries
    string entryNum;
    getline(iss, entryNum);

    // convert string to size_t
    size_t numOfEntriesSizeT = 0;
    if (!stringToSizeT(entryNum, &numOfEntriesSizeT))
        return false;

    d.entryNum = numOfEntriesSizeT;

    // block number
    string blockNumStr;
    getline(iss, blockNumStr);

    // convert string to size_t
    size_t blockNumSizeT = 0;
    if (!stringToSizeT(blockNumStr, &blockNumSizeT))
        return false;

    d.blockNum = blockNumSizeT;

    // entries
    for (size_t i = 0; i < d.entryNum; ++i)
    {
        // push back empty entry
        d.entries.push_back(directoryEntry());

        // file name
        getline(iss, d.entries[i].fileName);

        // is directory
        string isDir;
        getline(iss, isDir);
        d.entries[i].isDir = (isDir == "1");

        // length
        string length;
        getline(iss, length);
        // convert string to size_t
        size_t lengthSizeT = 0;
        if (!stringToSizeT(length, &lengthSizeT))
            return false;

        d.entries[i].file_size = lengthSizeT;

        // extension
        getline(iss, d.entries[i].extension);

        // time
        getline(iss, d.entries[i].time);

        // date
        getline(iss, d.entries[i].date);

        // block number
        string entryBlockNumStr;
        getline(iss, entryBlockNumStr);

        // convert string to size_t
        size_t entryBlockNumSizeT = 0;

        if (!stringToSizeT(entryBlockNumStr, &entryBlockNumSizeT))
            return false;

        d.entries[i].blockNum = entryBlockNumSizeT;

        // permissions
        string permissions;
        getline(iss, permissions);
        d.entries[i].permissions = stoi(permissions);

        // password
        getline(iss, d.entries[i].password);
    }

    return true;
}
bool fatTable::write_f_data(const block &fileBlock, string fileToWrite, size_t fileLength)
{
    // write file content to fileToWrite

    // get data from block
    vector<char> data = fileBlock.data;

    // convert data to string
    string content(data.begin(), data.end());

    block tempBlock = fileBlock;

    // complete the content
    while (tempBlock.nextBlock != -1)
    {
        // get next block
        block nextBlock;

        if (!search_block(tempBlock.nextBlock, nextBlock))
        {
            cerr << "Error: Could not find next block" << endl;
            return false;
        }

        // get data from next block
        data = nextBlock.data;
        // convert data to string
        string content2(data.begin(), data.end());

        // append content2 to content
        content += content2;

        tempBlock = nextBlock;
    }

    // get content as much as file length
    content = content.substr(0, fileLength);

    // write content to file
    ofstream file(fileToWrite, ios::binary);

    if (!file.is_open())
    {
        cerr << "Error: Could not open file: " << fileToWrite << endl;
        return false;
    }

    // write content to file
    file.write(content.c_str(), content.size());

    file.close();

    return true;
}

bool fatTable::rm_file(block &startBlock)
{
    // remove file starting from startBlock

    // get next block
    block nextBlock;
    if (startBlock.nextBlock == -1)
    {
        // remove start block and return
        if (!rm_block(startBlock))
        {
            cerr << "Error: Could not remove start block" << endl;
            return false;
        }

        return true;
    }

    if (!search_block(startBlock.nextBlock, nextBlock))
    {
        cerr << "Error: Could not find next block" << endl;
        return false;
    }

    // remove start block
    if (!rm_block(startBlock))
    {
        cerr << "Error: Could not remove start block" << endl;
        return false;
    }

    // recursive call
    return rm_file(nextBlock);
}

bool fatTable::rm_block(block &b)
{
    // remove block from blocks

    // get block number
    size_t blockNum = b.blockNum;

    // remove block from blocks
    for (size_t i = 0; i < fat12.blocks.size(); ++i)
    {
        if (fat12.blocks[i].blockNum == blockNum)
        {
            fat12.blocks.erase(fat12.blocks.begin() + i);
            break;
        }
    }

    b.nextBlock = -1;

    // add block to free blocks
    fat12.freeBlocks.push_back(b);

    // increase free block count
    fat12.sb.freeCount++;

    // decrease block count
    fat12.sb.blockCount--;

    return true;
}

bool fatTable::f_listDir(string path, string passW)
{
    if (!isInitialized)
        return false;

    // check if path is valid
    if (path[0] != '\\')
    {
        cerr << "Error: Invalid path" << endl;
        return false;
    }

    // get root position from superblock
    size_t rootP = fat12.sb.rootP;

    // get root directory
    block rootDirectory = fat12.blocks[rootP];

    // deserialize root directory
    directory root;

    if (!desrl_dir(root, rootDirectory))
    {
        cerr << "Error: Could not deserialize root directory" << endl;
        return false;
    }

    // search for directory in root directory
    directory d;

    // get directory name to be searched
    string directoryName = path.substr(path.find_last_of("\\") + 1);

    if (!search_dir(root, path, d, directoryName))
    {
        return false;
    }
    // check for directory permissions and password validation
    if ((d.permissions - READ_PERM) < 0)
    {
        cerr << "Error: Permission denied" << endl;
        return false;
    }
    if (!d.password.empty())
    {
        if (passW.empty())
        {
            cerr << "Directory is classified! Provide password!!" << endl;
            return false;
        }
        if (d.password != passW)
        {
            cerr << "Error: Incorrect password" << endl;
            return false;
        }
    }

    // print directory and its contents
    show_dir(d);

    return true;
}

bool fatTable::f_mkDir(string path, string passW)
{
    if (!isInitialized)
        return false;

    // check if path is valid
    if (path[0] != '\\')
    {
        cerr << "Error1: Invalid path" << endl;
        return false;
    }

    // cannot create root directory
    if (path == "\\")
    {
        cerr << "Error: Cannot create root directory" << endl;
        return false;
    }

    // are there any free blocks
    if (fat12.sb.freeCount == 0)
    {
        cerr << "Error: file system is full" << endl;
        return false;
    }

    string dirPath = path;

    // get root position from superblock
    size_t rootP = fat12.sb.rootP;

    // get root directory
    block rootDirectory = fat12.blocks[rootP];

    // deserialize root directory
    directory root;

    if (!desrl_dir(root, rootDirectory))
    {
        cerr << "Error: Could not deserialize root directory" << endl;
        return false;
    }

    // get directory name to be created
    string directoryName = path.substr(path.find_last_of("\\") + 1);

    // discard directory name from path
    path = path.substr(0, path.find_last_of("\\"));

    // get last directory name
    string lastDirectoryName = path.substr(path.find_last_of("\\") + 1);

    directory lastDirectory;

    // search for last directory
    if (!search_dir(root, path, lastDirectory, lastDirectoryName))
    {
        cerr << "Error: Directory not found" << endl;
        return false;
    }

    // check if directory already exists
    for (size_t i = 0; i < lastDirectory.entryNum; ++i)
    {
        if (lastDirectory.entries[i].fileName == directoryName)
        {
            cerr << "Error: Directory already exists" << endl;
            return false;
        }
    }
    if ((lastDirectory.permissions - WRITE_PERM) < 0) {
        cerr << "Error: Permission denied" << endl;
        return false;
    }

    if (!lastDirectory.password.empty() && (lastDirectory.password != passW)) {
        cerr << "Error: Incorrect password" << endl;
        return false;
    }
    // create directory entry
    directoryEntry dirEntry;
    dirEntry.fileName = directoryName;
    dirEntry.isDir = true;
    dirEntry.file_size = 0;
    dirEntry.extension = "0";
    dirEntry.time = "";
    dirEntry.date = "";

    if (fat12.freeBlocks.size() == 0)
    {
        cerr << "Error: File system is full" << endl;
        return false;
    }

    // get last block from free list
    block dirBlock = fat12.freeBlocks.back();

    // remove last block from free list
    fat12.freeBlocks.pop_back();

    // decrease free block count
    fat12.sb.freeCount--;

    // set next block to -1
    dirBlock.nextBlock = -1;

    // set directory block number
    dirEntry.blockNum = dirBlock.blockNum;

    // set time and date
    if (!set_modified(dirEntry.time, dirEntry.date))
    {
        cerr << "Error: Could not set time and date" << endl;
        return false;
    }

    // add directory entry to last directory
    lastDirectory.entries.push_back(dirEntry);

    // increase number of entries
    lastDirectory.entryNum++;

    // update last directory
    if (!updt_dir(lastDirectory))
    {
        cerr << "Error: Could not update last directory" << endl;
        return false;
    }

    // create directory
    directory newDirectory;
    newDirectory.dirPath = dirPath;
    newDirectory.entryNum = 0;
    newDirectory.blockNum = dirEntry.blockNum;
    newDirectory.entries = vector<directoryEntry>();

    // serialize directory
    vector<char> newDirectoryData;

    if (!srl_dir(newDirectory, newDirectoryData))
    {
        cerr << "Error: Could not serialize new directory" << endl;
        return false;
    }

    // set directory block data
    dirBlock.data = newDirectoryData;

    // add directory block to blocks
    fat12.blocks.push_back(dirBlock);

    // increase block count
    fat12.sb.blockCount++;

    // increase directory count
    fat12.sb.dirCount++;

    // update superblock
    if (!writeFile(fileSystemName))
    {
        cerr << "Error: Could not write to binary file" << endl;
        return false;
    }
    cout << "Dictory created" << endl;

    return true;
}

bool fatTable::f_rmDir(string path, string passW)
{
    if (!isInitialized)
        return false;

    // check if path is valid
    if (path[0] != '\\')
    {
        cerr << "Error: Invalid path" << endl;
        return false;
    }

    // cannot remove root directory
    if (path == "\\")
    {
        cerr << "Error: Cannot remove root directory" << endl;
        return false;
    }

    // get root position from superblock
    size_t rootP = fat12.sb.rootP;

    // get root directory
    block rootDirectory = fat12.blocks[rootP];

    // deserialize root directory
    directory root;

    if (!desrl_dir(root, rootDirectory))
    {
        cerr << "Error: Could not deserialize root directory" << endl;
        return false;
    }

    // get directory name to be removed
    string directoryName = path.substr(path.find_last_of("\\") + 1);

    // discard directory name from path
    path = path.substr(0, path.find_last_of("\\"));

    // get last directory name
    string lastDirectoryName = path.substr(path.find_last_of("\\") + 1);

    directory lastDirectory;

    // search for last directory
    if (!search_dir(root, path, lastDirectory, lastDirectoryName))
    {
        cerr << "Error: Directory not found" << endl;
        return false;
    }
    if ((lastDirectory.permissions - WRITE_PERM) < 0) {
        cerr << "Error: Permission denied" << endl;
        return false;
    }

    if (!lastDirectory.password.empty() && (lastDirectory.password != passW)) {
        cerr << "Error: Incorrect password" << endl;
        return false;
    }
    // check if directory exists
    bool exists = false;
    size_t index = 0;

    for (size_t i = 0; i < lastDirectory.entryNum; ++i)
    {
        if (lastDirectory.entries[i].fileName == directoryName &&
            lastDirectory.entries[i].isDir == true)
        {
            exists = true;
            index = i;
            break;
        }
    }

    if (!exists)
    {
        cerr << "Error: Directory not found" << endl;
        return false;
    }

    // get directory entry
    directoryEntry dirEntry = lastDirectory.entries[index];

    // get directory block
    block dirBlock;

    if (!search_block(dirEntry.blockNum, dirBlock))
    {
        cerr << "Error: Could not find directory block" << endl;
        return false;
    }

    // deserialize directory
    directory dir;

    if (!desrl_dir(dir, dirBlock))
    {
        cerr << "Error: Could not deserialize directory" << endl;
        return false;
    }

    // check if directory is empty
    if (dir.entryNum != 0)
    {
        cerr << "Error: Directory is not empty" << endl;
        return false;
    }
    else
    {
        // remove directory entry from last directory
        lastDirectory.entries.erase(lastDirectory.entries.begin() + index);

        // decrease number of entries
        lastDirectory.entryNum--;

        // update last directory
        if (!updt_dir(lastDirectory))
        {
            cerr << "Error: Could not update last directory" << endl;
            return false;
        }

        block removed;
        size_t removedIndex = 0;
        // remove directory block from blocks
        for (block b : fat12.blocks)
        {
            if (b.blockNum == dirEntry.blockNum)
            {
                // delete block
                removed = b;
                fat12.blocks.erase(fat12.blocks.begin() + removedIndex);
            }
            removedIndex++;
        }

        // add removed block to free list
        fat12.freeBlocks.push_back(removed);

        // increase free block count
        fat12.sb.freeCount++;

        // decrease block count
        fat12.sb.blockCount--;

        // decrease directory count
        fat12.sb.dirCount--;

        // update superblock
        if (!writeFile(fileSystemName))
        {
            cerr << "Error: Could not write to binary file" << endl;
            return false;
        }
    }

    return true;
}

bool fatTable::f_dumpe2fs()
{
    if (!isInitialized)
        return false;

    // print file system information
    cout << "\n Current File System FAT12\n"<< endl;
    cout << "Block Size: " << fat12.sb.blockSize << " KB" << endl;
    cout << "Block Count: " << fat12.sb.blockCount << endl;
    cout << "Number of Blocks: " << fat12.sb.numOfBlocks << endl;
    cout << "Number of Free Blocks: " << fat12.sb.freeCount << endl;
    cout << "Number Of Files: " << fat12.sb.fileCount << endl;
    cout << "Number Of Directories: " << fat12.sb.dirCount << endl;

    return true;
}

bool fatTable::f_write(string path, string fileToRead,string passW){
    if (!isInitialized)
        return false;

    // check if path is valid
    if (path[0] != '\\'){
        cerr << "Invalid path!!!" << endl;
        return false;
    }
    
    // get root position from superblock
    size_t rootP = fat12.sb.rootP;

    // get root directory
    block rootDirectory = fat12.blocks[rootP];

    // deserialize root directory
    directory root;

    if (!desrl_dir(root, rootDirectory)){
        cerr << "Could not deserialize root directory" << endl;
        return false;
    }

    // get file name to be created
    string fileName = path.substr(path.find_last_of("\\") + 1);

    // discard file name from path
    path = path.substr(0, path.find_last_of("\\"));

    // get last directory name
    string lastDirectoryName = path.substr(path.find_last_of("\\") + 1);

    directory lastDirectory;

    // search for last directory
    if (!search_dir(root, path, lastDirectory, lastDirectoryName)){
        cerr << "Directory not found" << endl;
        return false;
    }

    bool exists = false;
    directoryEntry fileEntry;
    size_t index = 0;

    // check if file exists
    for (size_t i = 0; i < lastDirectory.entryNum; ++i){
        if (lastDirectory.entries[i].isDir == false &&
            lastDirectory.entries[i].fileName == fileName)
        {
            exists = true;
            index = i;
            break;
        }
    }

    // if file already exists, overwrite the file
    if (exists){
        // get file entry
        fileEntry = lastDirectory.entries[index];
        if ((fileEntry.permissions-WRITE_PERM) < 0) {
            cerr << "Error: Permission denied" << endl;
            return false;
        }
        //if password provided, check for password validation
        if (!fileEntry.password.empty()) {
            if (passW.empty()) {
                cerr << "File is classified! Provide password!!" << endl;
                return false;
            }
            if (fileEntry.password != passW) {
                cerr << "Error: Incorrect password" << endl;
                return false;
            }
        }
        //operate write if password is valid

        // set time and date
        if (!set_modified(fileEntry.time, fileEntry.date)){
            cerr << "Error: Could not set time and date" << endl;
            return false;
        }

        // get file block
        block fileBlock;

        if (!search_block(fileEntry.blockNum, fileBlock)){
            cerr << "Error: Could not find file block" << endl;
            return false;
        }
        // deserialize file
        file fileToOverwrite;

        fileToOverwrite.fileName = fileName;
        fileToOverwrite.fileDate = fileEntry.date;
        fileToOverwrite.fileTime = fileEntry.time;
        fileToOverwrite.permissions = fileEntry.permissions;
        fileToOverwrite.password = fileEntry.password;


        // serialize file
        vector<char> newFileData;

        if (!srl_file(fileToOverwrite, fileToRead, newFileData)){
            cerr << "Error: Could not serialize new file" << endl;
            return false;
        }

        // update file entry length
        fileEntry.file_size = fileToOverwrite.fileLength;

        // set file entry to last directory
        lastDirectory.entries[index] = fileEntry;

        // update last directory
        if (!updt_dir(lastDirectory)){
            cerr << "Error: Could not update last directory" << endl;
            return false;
        }

        // check if file fits in the same block
        if (newFileData.size() <= fat12.sb.blockSize)
        {
            // set file block data
            fileBlock.data = newFileData;

            // set file block to blocks
            if (!init_block(fileEntry.blockNum, fileBlock))
            {
                cerr << "Error: Could not set file block" << endl;
                return false;
            }
        }
        else{
            // add file data to current block as much as possible
            for (size_t i = 0; i < fat12.sb.blockSize; i++) {
                fileBlock.data[i] = newFileData[i];
            }

            size_t remainingBytes = newFileData.size() - fat12.sb.blockSize;
            // discard new file data that is already written
            newFileData.erase(newFileData.begin(), newFileData.begin() + fat12.sb.blockSize);

            bool done = false;
            while (true) {
                // check if there is a next block
                if (fileBlock.nextBlock == -1 && !done){

                    // check if there are any free blocks
                    if (fat12.freeBlocks.size() == 0)
                    {
                        cerr << "Error: File system is full" << endl;
                        return false;
                    }

                    // get next block from free list
                    block nextBlock = fat12.freeBlocks.back();

                    // remove last block from free list
                    fat12.freeBlocks.pop_back();

                    // decrease free block count
                    fat12.sb.freeCount--;

                    // set next block to -1
                    nextBlock.nextBlock = -1;

                    // set next block of current block
                    fileBlock.nextBlock = nextBlock.blockNum;

                    // set file block to blocks
                    if (!init_block(fileBlock.blockNum, fileBlock))
                    {
                        cerr << "Error: Could not set file block" << endl;
                        return false;
                    }

                    // set file block number
                    fileBlock = nextBlock;

                    // add file data to current block as much as possible
                    for (size_t i = 0; i < fat12.sb.blockSize; i++)
                    {
                        fileBlock.data[i] = newFileData[i];
                    }

                    // // discard new file data that is already written
                    newFileData.erase(newFileData.begin(), newFileData.begin() + fat12.sb.blockSize);

                    // add next block to blocks
                    fat12.blocks.push_back(fileBlock);

                    // increase block count
                    fat12.sb.blockCount++;
                }
                else if (!done) {
                    // set file block to blocks
                    if (!init_block(fileBlock.blockNum, fileBlock))
                    {
                        cerr << "Error: Could not set file block" << endl;
                        return false;
                    }

                    // get next block
                    if (!search_block(fileBlock.nextBlock, fileBlock))
                    {
                        cerr << "Error: Could not find file block" << endl;
                        return false;
                    }

                    // add file data to current block as much as possible
                    for (size_t i = 0; i < fat12.sb.blockSize; i++)
                    {
                        fileBlock.data[i] = newFileData[i];
                    }

                    // discard new file data that is already written
                    newFileData.erase(newFileData.begin(), newFileData.begin() + fat12.sb.blockSize);
                }

                if (done)
                    break;

                // consider underflow
                if (remainingBytes <= fat12.sb.blockSize)
                {
                    done = true;
                    continue;
                }

                remainingBytes -= fat12.sb.blockSize;
            }
        }

        // update superblock
        if (!writeFile(fileSystemName))
        {
            cerr << "Error: Could not write to binary file" << endl;
            return false;
        }
        cout<<fileName<<" Updated"<<endl;

    }
    else
    {
        // create file entry object for the new file
        directoryEntry fileEntry;
        fileEntry.fileName = fileName;
        fileEntry.isDir = false;
        fileEntry.file_size = 0;
        fileEntry.extension = "0";
        fileEntry.time = "";
        fileEntry.date = "";
        fileEntry.permissions = 0700; // Default permissions
        fileEntry.password = passW; 

        if (fat12.freeBlocks.size() == 0)
        {
            cerr << "Error: File system is full" << endl;
            return false;
        }

        // get last block from free list
        block fileBlock = fat12.freeBlocks.back();

        // remove last block from free list
        fat12.freeBlocks.pop_back();

        // decrease free block count
        fat12.sb.freeCount--;

        // set next block to -1
        fileBlock.nextBlock = -1;

        // set file block number
        fileEntry.blockNum = fileBlock.blockNum;

        // set time and date
        if (!set_modified(fileEntry.time, fileEntry.date))
        {
            cerr << "Error: Could not set time and date" << endl;
            return false;
        }

        // create file
        file newFile;
        newFile.fileName = fileName;
        newFile.fileLength = 0;
        newFile.blockNum = fileEntry.blockNum;

        // set time and date
        newFile.fileTime = fileEntry.time;
        newFile.fileDate = fileEntry.date;
         newFile.permissions = fileEntry.permissions;
        newFile.password = fileEntry.password;

        // serialize file
        vector<char> newFileData;

        if (!srl_file(newFile, fileToRead, newFileData))
        {
            cerr << "Error: Could not serialize new file" << endl;
            return false;
        }

        // update file entry length
        fileEntry.file_size = newFile.fileLength;
        newFile.fileLength = newFile.fileLength;

        // add file entry to last directory
        lastDirectory.entries.push_back(fileEntry);

        // increase number of entries
        lastDirectory.entryNum++;

        // update last directory
        if (!updt_dir(lastDirectory))
        {
            cerr << "Error: Could not update last directory" << endl;
            return false;
        }

        // if file fits in the same block
        if (newFileData.size() <= fat12.sb.blockSize)
        {
            // set file block data
            fileBlock.data = newFileData;

            // add file block to blocks
            fat12.blocks.push_back(fileBlock);
            fat12.sb.blockCount++;
        }
        else
        {

            fat12.sb.blockCount++;
            // add file data to current block as much as possible
            for (size_t i = 0; i < fat12.sb.blockSize; i++)
            {
                fileBlock.data[i] = newFileData[i];
            }

            // discard new file data that is already written
            newFileData.erase(newFileData.begin(), newFileData.begin() + fat12.sb.blockSize);

            fat12.blocks.push_back(fileBlock);
            size_t remainingBytes = newFileData.size() - fat12.sb.blockSize;
            bool done = false;
            while (true)
            {
                if (fat12.freeBlocks.size() == 0)
                {
                    cerr << "Error: File system is full" << endl;
                    return false;
                }

                // get next block from free list
                block nextBlock = fat12.freeBlocks.back();

                // remove last block from free list
                fat12.freeBlocks.pop_back();

                // decrease free block count
                fat12.sb.freeCount--;

                // set next block to -1
                nextBlock.nextBlock = -1;

                // set next block of current block
                fileBlock.nextBlock = nextBlock.blockNum;

                // set file block to blocks
                if (!init_block(fileBlock.blockNum, fileBlock))
                {
                    cerr << "Error: Could not set file block" << endl;
                    return false;
                }

                // set file block number
                fileBlock = nextBlock;

                // add file data to current block as much as possible
                for (size_t i = 0; i < fat12.sb.blockSize; i++)
                {
                    fileBlock.data[i] = newFileData[i];
                }

                // discard new file data that is already written
                if (!done)
                    newFileData.erase(newFileData.begin(), newFileData.begin() + fat12.sb.blockSize);

                // add next block to blocks
                fat12.blocks.push_back(fileBlock);

                // increase block count
                fat12.sb.blockCount++;

                if (done)
                    break;

                // consider underflow
                if (remainingBytes <= fat12.sb.blockSize)
                {
                    done = true;
                    continue;
                }

                remainingBytes -= fat12.sb.blockSize;
            }
        }

        // increase file count
        fat12.sb.fileCount++;

        // update superblock
        if (!writeFile(fileSystemName))
        {
            cerr << "Error: Could not write to binary file" << endl;
            return false;
        }
    }

    return true;
}

// write the content of a file in the path to fileToWrite
bool fatTable::f_read(string path, string fileToWrite, string passW)
{
    if (!isInitialized)
        return false;

    // check if path is valid
    if (path[0] != '\\')
    {
        cerr << "Error: Invalid path" << endl;
        return false;
    }

    // get root position from superblock
    size_t rootP = fat12.sb.rootP;

    // get root directory
    block rootDirectory = fat12.blocks[rootP];

    // deserialize root directory
    directory root;

    if (!desrl_dir(root, rootDirectory))
    {
        cerr << "Error: Could not deserialize root directory" << endl;
        return false;
    }

    // get file name to be created
    string fileName = path.substr(path.find_last_of("\\") + 1);

    // discard file name from path
    path = path.substr(0, path.find_last_of("\\"));

    // get last directory name
    string lastDirectoryName = path.substr(path.find_last_of("\\") + 1);

    directory lastDirectory;

    // search for last directory
    if (!search_dir(root, path, lastDirectory, lastDirectoryName))
    {
        cerr << "Error: Directory not found" << endl;
        return false;
    }

    directoryEntry fileEntry;
    bool exists = false;
    // check if file exists, if it does not, return false
    for (size_t i = 0; i < lastDirectory.entryNum; ++i)
    {
        if (lastDirectory.entries[i].isDir == false &&
            lastDirectory.entries[i].fileName == fileName)
        {
            fileEntry = lastDirectory.entries[i];
            exists = true;
            break;
        }
    }

    if (!exists)
    {
        cerr << "Error: File does not exist" << endl;
        return false;
    }

    if ((fileEntry.permissions - READ_PERM) < 0)
    {
        cerr << "Error: Permission denied" << endl;
        return false;
    }
    // if password provided, check for password validation
    if (!fileEntry.password.empty())
    {
        if (passW.empty())
        {
            cerr << "File is classified! Provide password!!" << endl;
            return false;
        }
        if (fileEntry.password != passW)
        {
            cerr << "Error: Incorrect password" << endl;
            return false;
        }
    }

    // get file block
    block fileBlock;

    if (!search_block(fileEntry.blockNum, fileBlock))
    {
        cerr << "Error: Could not find file block" << endl;
        return false;
    }

    // deserialize file, and write to fileToWrite
    if (!write_f_data(fileBlock, fileToWrite, fileEntry.file_size))
    {
        cerr << "Error: Could not write file content" << endl;
        return false;
    }

    return true;
}
//Deletes file from the path.This again works like toLinux del command. 
bool fatTable::f_del(string path, string passW)
{
    if (!isInitialized)
        return false;

    // check if path is valid
    if (path[0] != '\\')
    {
        cerr << "Error: Invalid path" << endl;
        return false;
    }

    // cannot remove root directory
    if (path == "\\")
    {
        cerr << "Error: Cannot remove root directory" << endl;
        return false;
    }

    // get root position from superblock
    size_t rootP = fat12.sb.rootP;

    // get root directory
    block rootDirectory = fat12.blocks[rootP];

    // deserialize root directory
    directory root;

    if (!desrl_dir(root, rootDirectory))
    {
        cerr << "Error: Could not deserialize root directory" << endl;
        return false;
    }

    // get file name to be removed
    string fileName = path.substr(path.find_last_of("\\") + 1);

    // discard file name from path
    path = path.substr(0, path.find_last_of("\\"));

    // get last directory name
    string lastDirectoryName = path.substr(path.find_last_of("\\") + 1);
    directory lastDirectory;

    // search for last directory
    if (!search_dir(root, path, lastDirectory, lastDirectoryName))
    {
        cerr << "Error: Directory not found" << endl;
        return false;
    }

    // check if file exists
    bool exists = false;
    size_t index = 0;

    for (size_t i = 0; i < lastDirectory.entryNum; ++i)
    {
        if (lastDirectory.entries[i].fileName == fileName && lastDirectory.entries[i].isDir == false)
        {
            exists = true;
            index = i;
            break;
        }
    }

    if (!exists)
    {
        cerr << "Error: File not found" << endl;
        return false;
    }

    // get file entry
    directoryEntry fileEntry;

    for (size_t i = 0; i < lastDirectory.entryNum; ++i)
    {
        if (lastDirectory.entries[i].fileName == fileName)
        {
            fileEntry = lastDirectory.entries[i];
            break;
        }
    }
    if ((fileEntry.permissions - WRITE_PERM) < 0)
    {
        cerr << "Error: Permission denied" << endl;
        return false;
    }
    // if password provided, check for password validation
    if (!fileEntry.password.empty())
    {
        if (passW.empty())
        {
            cerr << "File is classified! Provide password!!" << endl;
            return false;
        }
        if (fileEntry.password != passW)
        {
            cerr << "Error: Incorrect password" << endl;
            return false;
        }
    }
    // get file block
    block fileBlock;
    if (!search_block(fileEntry.blockNum, fileBlock))
    {
        cerr << "Error: Could not find file block" << endl;
        return false;
    }

    // remove file entry from last directory
    lastDirectory.entries.erase(lastDirectory.entries.begin() + index);

    // decrease number of entries
    lastDirectory.entryNum--;

    // update last directory
    if (!updt_dir(lastDirectory))
    {
        cerr << "Error: Could not update last directory" << endl;
        return false;
    }

    // remove file block, and its next blocks from blocks, block count and free block count updated in rm_file
    if (!rm_file(fileBlock))
    {
        cerr << "Error: Could not remove file" << endl;
        return false;
    }

    // decrease file count
    fat12.sb.fileCount--;

    // update superblock
    if (!writeFile(fileSystemName))
    {
        cerr << "Error: Could not write to binary file" << endl;
        return false;
    }

    return true;
}

bool fatTable::f_chmod(std::string path, std::string permissions)
{
    if (!isInitialized) {
        std::cerr << "Error: File system not initialized" << std::endl;
        return false;
    }

    // Check if path is valid
    if (path.empty() || path[0] != '\\') {
        std::cerr << "Error: Invalid path" << std::endl;
        return false;
    }

    // Cannot change permissions of root directory
    if (path == "\\") {
        std::cerr << "Error: Cannot change permissions of root directory" << std::endl;
        return false;
    }

    // Get root position from superblock
    size_t rootP = fat12.sb.rootP;

    // Get root directory
    block rootDirectory = fat12.blocks[rootP];

    // Deserialize root directory
    directory root;
    if (!desrl_dir(root, rootDirectory)) {
        std::cerr << "Error: Could not deserialize root directory" << std::endl;
        return false;
    }

    // Get file name
    std::string fileName = path.substr(path.find_last_of("\\") + 1);

    // Discard file name from path
    path = path.substr(0, path.find_last_of("\\"));

    // Get last directory name
    std::string lastDirectoryName = path.substr(path.find_last_of("\\") + 1);
    directory lastDirectory;

    // Search for last directory
    if (!search_dir(root, path, lastDirectory, lastDirectoryName)) {
        std::cerr << "Error: Directory not found" << std::endl;
        return false;
    }

    // Check if file exists
    bool exists = false;
    directoryEntry* targetFile = nullptr;

    for (size_t i = 0; i < lastDirectory.entryNum; ++i) {
        if (lastDirectory.entries[i].fileName == fileName && !lastDirectory.entries[i].isDir) {
            targetFile = &lastDirectory.entries[i];
            exists = true;
            break;
        }
    }

    if (!exists) {
        std::cerr << "Error: File not found" << std::endl;
        return false;
    }
    if ((targetFile->permissions - WRITE_PERM) <0) {
        cerr << "Error: Permission denied" << endl;
        return false;
    }

    // Parse and set permissions
    targetFile->permissions = 0;
    for (char perm : permissions) {
        switch (perm) {
            case '+':
                break;
            case 'r':
                targetFile->permissions |= READ_PERM;
                break;
            case 'w':
                targetFile->permissions |= WRITE_PERM;
                break;
            case 'x':
                targetFile->permissions |= EXECUTE_PERM;
                break;
            case '-': // set to no perms
                targetFile->permissions = 0;
                break;
            default:
                std::cerr << "Error: Invalid permission" << std::endl;
                return false;
        }
    }

    // Update last directory
    if (!updt_dir(lastDirectory)) {
        std::cerr << "Error: Could not update last directory" << std::endl;
        return false;
    }

    // Write changes to file
    if (!writeFile(fileSystemName)) {
        std::cerr << "Error: Could not write to binary file" << std::endl;
        return false;
    }

    return true;
}
bool fatTable::f_addpw(string path, string password) {
    if (!isInitialized) {
        cerr << "Error: File system not initialized" << endl;
        return false;
    }

    if (path[0] != '\\') {
        cerr << "Error: Invalid path" << endl;
        return false;
    }

    size_t rootP = fat12.sb.rootP;
    block rootDirectory = fat12.blocks[rootP];
    directory root;

    if (!desrl_dir(root, rootDirectory)) {
        cerr << "Error: Could not deserialize root directory" << endl;
        return false;
    }

    string fileName = path.substr(path.find_last_of("\\") + 1);
    path = path.substr(0, path.find_last_of("\\"));
    string lastDirectoryName = path.substr(path.find_last_of("\\") + 1);
    directory lastDirectory;

    if (!search_dir(root, path, lastDirectory, lastDirectoryName)) {
        cerr << "Error: Directory not found" << endl;
        return false;
    }

    bool exists = false;
    size_t index = 0;
    for (size_t i = 0; i < lastDirectory.entryNum; ++i) {
        if (lastDirectory.entries[i].fileName == fileName) {
            exists = true;
            index = i;
            break;
        }
    }

    if (!exists) {
        cerr << "Error: File not found" << endl;
        return false;
    }
    if ((lastDirectory.entries[index].permissions - WRITE_PERM) < 0) {
        cerr << "Error: Permission denied" << endl;
        return false;
    }


    lastDirectory.entries[index].password = password;

    if (!updt_dir(lastDirectory)) {
        cerr << "Error: Could not update directory" << endl;
        return false;
    }
    if (!writeFile(fileSystemName)) {
        std::cerr << "Password: Could not write to binary file" << std::endl;
        return false;
    }
    return true;
}