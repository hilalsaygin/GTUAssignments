#include "include/fat12.h"

using namespace filesystem12;


int main (int argc, char** argv)
{

    if (argc != 3){
        std::cerr << "Usage: ./makeFileSystem <block size> <file name>" << std::endl;
        return 1;
    }

    // create a fat12 object
    fatTable *fat12 = new fatTable();

    // initialize the fat12 system
    if (!fat12->init(argv[1]))
    {
        std::cerr << "Error: Invalid block size" << std::endl;
        return 1;
    }

    // give filename into fat12 object
    fat12->writeFile(argv[2]);

    // delete the fat12 object
    delete fat12;

    return 0;
}