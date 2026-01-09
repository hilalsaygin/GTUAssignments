
#include <common/types.h>
#include <gdt.h>
#include <hardwarecommunication/interrupts.h>
#include <hardwarecommunication/pci.h>
#include <drivers/driver.h>
#include <drivers/keyboard.h>
#include <drivers/mouse.h>
#include <drivers/vga.h>
#include <gui/desktop.h>
#include <gui/window.h>
#include <multitasking.h>
#include <syscalls.h>

// #define GRAPHICSMODE
using namespace myos;
using namespace myos::common;
using namespace myos::drivers;
using namespace myos::hardwarecommunication;
using namespace myos::gui;


char* itoa(int value, char* result, int base) {
    // check that the base if valid
    if (base < 2 || base > 36) { *result = '\0'; return result; }

    char* ptr = result, *ptr1 = result, tmp_char;
    int tmp_value;

    do {
        tmp_value = value;
        value /= base;
        *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqrstuvwxyz" [35 + (tmp_value - value * base)];
    } while ( value );

    // Apply negative sign
    if (tmp_value < 0) *ptr++ = '-';
    *ptr-- = '\0';
    while(ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr--= *ptr1;
        *ptr1++ = tmp_char;
    }
    return result;
}

void printf(char* str)
{
    static uint16_t* VideoMemory = (uint16_t*)0xb8000;

    static uint8_t x=0,y=0;

    for(int i = 0; str[i] != '\0'; ++i)
    {
        switch(str[i])
        {
            case '\n':
                x = 0;
                y++;
                break;
            case '\t':
                x = x+5;
                if(x>=80){
                    x=0;
                }
            break;
            default:
                VideoMemory[80*y+x] = (VideoMemory[80*y+x] & 0xFF00) | str[i];
                x++;
                break;
        }

        if(x >= 80)
        {
            x = 0;
            y++;
        }

        if(y >= 25)
        {
            for(y = 0; y < 25; y++)
                for(x = 0; x < 80; x++)
                    VideoMemory[80*y+x] = (VideoMemory[80*y+x] & 0xFF00) | ' ';
            x = 0;
            y = 0;
        }
    }
}

void printNum(int num){
    char numberStr[10];
    itoa(num,numberStr,10);
    printf(numberStr);
}

void printfHex(uint8_t key)
{
    char* foo = "00";
    char* hex = "0123456789ABCDEF";
    foo[0] = hex[(key >> 4) & 0xF];
    foo[1] = hex[key & 0xF];
    printf(foo);
}

void printArray(int arr[],int n){
    printf("Input: {");
    for(int i=0;i<n;i++){
        printNum(arr[i]);
        if(i+1!=n){
            printf(",");
        }
    }
    printf("} ");
}

class PrintfKeyboardEventHandler : public KeyboardEventHandler
{
public:
    void OnKeyDown(char c)
    {
        char* foo = " ";
        foo[0] = c;
        printf(foo);
    }
};

class MouseToConsole : public MouseEventHandler
{
    int8_t x, y;
public:
    
    MouseToConsole()
    {
        uint16_t* VideoMemory = (uint16_t*)0xb8000;
        x = 40;
        y = 12;
        VideoMemory[80*y+x] = (VideoMemory[80*y+x] & 0x0F00) << 4
                            | (VideoMemory[80*y+x] & 0xF000) >> 4
                            | (VideoMemory[80*y+x] & 0x00FF);        
    }
    
    virtual void OnMouseMove(int xoffset, int yoffset)
    {
        static uint16_t* VideoMemory = (uint16_t*)0xb8000;
        VideoMemory[80*y+x] = (VideoMemory[80*y+x] & 0x0F00) << 4
                            | (VideoMemory[80*y+x] & 0xF000) >> 4
                            | (VideoMemory[80*y+x] & 0x00FF);

        x += xoffset;
        if(x >= 80) x = 79;
        if(x < 0) x = 0;
        y += yoffset;
        if(y >= 25) y = 24;
        if(y < 0) y = 0;

        VideoMemory[80*y+x] = (VideoMemory[80*y+x] & 0x0F00) << 4
                            | (VideoMemory[80*y+x] & 0xF000) >> 4
                            | (VideoMemory[80*y+x] & 0x00FF);
    }
    
};

unsigned int xorshift32(unsigned int *state) {
    unsigned int x = *state;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    *state = x;
    return x;
}

int32_t randGenerator(int min, int max)
{
    uint64_t counter;
    int32_t num;
    /* Read the clock counter */
    asm("rdtsc": "=A"(counter));

    /* Use the clock counter as a source of randomness */
    counter = counter * 1103515245 + 12345;
    num = (int)(counter / 65536) % (max - min);
    if (num<0)
        num+=max;
    return num+min;
}
//implement collatz sequence finding operation
void collatz(int n) {
    printNum(n);
    printf(" : ");
    while (n != 1) {
        if (n % 2 == 0) {
            n /= 2;
        }
        else {
            n = 3 * n + 1;
        }
        printNum(n);
        printf(" ");
    }

    printf("\n");
}


//function that implements the long running operation
int long_running_program(int n) {
    int result = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            result += i * j;
        }
    }
    return result;
}


//********* TEST FUNCTIONS ************
//test fucntion that is calling the long running task

void longRunningTask() {
    int result = long_running_program(1000);
    printf("**Result of long-running: ");
    printNum(result);
    printf("\n");
    exit_call();
}
void collatzTask(){
    //find collatz sequence for input of 9 number
    int n = randGenerator(0,100);
    printf("**Collatz Sequences-> ");

    collatz(n);
    exit_call();
}


void initKernelA()
{
    // Load and start collatzTask 3 times
    
    uint32_t pid1 = addTask(collatzTask);
    uint32_t pid2 = addTask(collatzTask);
    uint32_t pid3 = addTask(collatzTask);

    // Load and start long_running_program 3 times
    uint32_t pid4 = addTask(longRunningTask);
    uint32_t pid5 = addTask(longRunningTask);
    uint32_t pid6 = addTask(longRunningTask);
    waitpid(pid6);
    exit_call();

    
}
 
void forkTestExample()
{
     int parent_pid = getPid();
    printf("Parent pID:");
    printNum(parent_pid);
    printf("\n");

    int child_pid = 0;
    fork(&child_pid);
    if (child_pid == 0)
    {
        printf("Running child process --");
        printf("Parent pID:");
        printNum(parent_pid);
        printf("\n");
    }
    else
    {
        printf("Running parent process --");
        printf("Child pID: ");
        printNum(child_pid);
        printf("\n");
    }
    
    exit_call();
}
void initFork(){
    uint32_t pid = addTask(forkTestExample);
    waitpid(pid);
    exit_call();
}

void execTestExamle1(){
    printf("Calculating with exec process "); printNum(getPid()); 
    printf(" The sum is 987.\n");
    exit_call();
}

void execTestExamle()
{
    printf(" Testing for exec() ! "); printNum(getPid()); printf(" is starting\n");
    int exec1= exec(execTestExamle1);
    printf(" exec Process "); printNum(getPid()); printf(" finished.\n");
    exit_call();
}

typedef void (*constructor)();
extern "C" constructor start_ctors;
extern "C" constructor end_ctors;
extern "C" void callConstructors()
{
    for(constructor* i = &start_ctors; i != &end_ctors; i++)
        (*i)();
}



extern "C" void kernelMain(const void* multiboot_structure, uint32_t /*multiboot_magic*/)
{
    printf("Hello my OS for kernel A --- Testting for OS \n");

    GlobalDescriptorTable gdt;
    TaskManager taskManager(&gdt);

    // ************** Test Cases **************

    // Testing collatz and long running scenario
    // Task task1(&gdt,initKernelA);
    // taskManager.AddTask(&task1);

    
    // test Exec implementation
    // Task task2(&gdt,execTestExamle);
    // taskManager.AddTask(&task2);

    //Test fork function
    // Task task3(&gdt,initFork);
    // taskManager.AddTask(&task3);
    Task task3(&gdt,forkTestExample);
    taskManager.AddTask(&task3);
    
    InterruptManager interrupts(0x20, &gdt, &taskManager);
    SyscallHandler syscalls(&interrupts, 0x80);
    


    interrupts.Activate();
    
    while(1)
    {
    
    }
}
