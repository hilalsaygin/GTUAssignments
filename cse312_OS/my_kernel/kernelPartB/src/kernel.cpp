
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

int binary_search(int arr[], int left, int right, int key) {
    while (left <= right) {
        int mid = left + (right - left) / 2;
        
        if (arr[mid] == key) {
            return mid;
        }
        else if (arr[mid] < key) {
            left = mid + 1;
        }
        else {
            right = mid - 1;
        }
    }

    return -1;
}

int linear_search(int arr[], int n, int key) {
    for (int i = 0; i < n; i++) {
        if (arr[i] == key) {
            return i;
        }
    }
    return -1;
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

void TaskLinearSearch(){
    //initialize the number array values given in the pdf 
        int x = 175;

    int arr[] = {10, 20, 80, 30, 60, 50, 110, 100, 130, 170};
    int n = sizeof(arr) / sizeof(arr[0]);
    //apply linear search with array and the x
    int result = linear_search(arr, n, x);
    printf("**LinearSearch ");
    printArray(arr,n);
    printf("x = ");
    printNum(x);
    printf("; Output: ");
    printNum(result);
    printf("\n");
    
    exit_call();
}

void TaskBinarySearch(){
    int arr[] = {10, 20, 80, 30, 60, 50, 110, 100, 130, 170};
    int n = sizeof(arr) / sizeof(arr[0]);
    int x = 110;
    int result = binary_search(arr, 0, n - 1, x);
    printf("**BinarySearch ");

    printArray(arr,n);
    printf("x = ");
    printNum(x);
    printf("; Output: ");
    printNum(result);
    printf("\n");

    exit_call();
}


void forkTestExample()
{
    int parentPid=getPid();
    printf("Parent pID:");
    printNum(parentPid);
    printf("\n");
    int childPid=0;
    fork(&childPid);
    if(childPid==0){
        printf("Parent Process ID: ");
        printNum(parentPid);
        printf("\n");
    }else{
        printf("Child Process ID: ");
        printNum(getPid());
        printf("\n");
    }

    printf("Ending Parent Process :");
    printNum(getPid());
    printf("\n");
    exit_call();
}

void execTestExamle1(){
    printf("Calculating with exec process "); printNum(getPid()); 
    printf(" The sum is 987.\n");
    exit_call();
}

void execTestExamle()
{
    printf("Testing for exec() ! "); printNum(getPid()); printf(" is starting\n");
    int exec1=exec(execTestExamle1);
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


void initStrategy1()
{
    uint32_t pid = 0;
    int rnd= randGenerator(0,3);
    for (int i = 0;  i < 10; i++){
        if (rnd == 0)
            pid = addTask(TaskLinearSearch);

        else if( rnd ==1)
            pid = addTask(TaskBinarySearch);
        else if (rnd == 2)  
            pid = addTask(collatzTask);
        else
            pid = addTask(longRunningTask);

    }
    waitpid(pid);
    exit_call();
}
void initStrategy2()
{
    uint32_t pid1 = 0;
    uint32_t pid2 = 0;
    uint32_t pid3 = 0;
    //chose 2 out of 4 program randomly and load them into the kernel 3times using systemcall 
    for (int i = 0;  i < 2; i++){
        int rnd= randGenerator(0,3);
        if (rnd == 0){
            pid1 = addTask(TaskLinearSearch);
            pid2 = addTask(TaskLinearSearch);
            pid3 = addTask(TaskLinearSearch);
        }
        else if (rnd == 1){
            pid1 = addTask(TaskBinarySearch);
            pid2 = addTask(TaskBinarySearch);
            pid3 = addTask(TaskBinarySearch);
        }
        else if (rnd == 0){
            pid1 = addTask(collatzTask);
            pid2 = addTask(collatzTask);
            pid3 = addTask(collatzTask);
        }
        else{
            pid1 = addTask(longRunningTask);
            pid2 = addTask(longRunningTask);
            pid3 = addTask(longRunningTask);
        
        }

    }
    waitpid(pid1);
    waitpid(pid2);
    waitpid(pid3);
    exit_call();
}
// void initStrategy3()
// {

// }
// void initStrategy4()
// {

// }

extern "C" void kernelMain(const void* multiboot_structure, uint32_t /*multiboot_magic*/)
{
    printf("Hello my OS for kernel B --- Testting \n");

    GlobalDescriptorTable gdt;
    TaskManager taskManager(&gdt);
    // ************** Test Cases **************

    // task object for the firt strategy
    // Task task1(&gdt,initStrategy1);
    // taskManager.AddTask(&task1);

    //create taks object for the second strategy
    Task task2(&gdt,initStrategy2);
    taskManager.AddTask(&task2);
    
    // Task for 3.Stratgy
    // Task task3(&gdt, initStrategy3, HIGH); // create task with priority
    // taskManager.AddTask(&task3);

    // Task for 4.strategy
    // Task task4(&gdt,initStrategy4);
    // taskManager.AddTask(&task4);
    
    InterruptManager interrupts(0x20, &gdt, &taskManager);
    SyscallHandler syscalls(&interrupts, 0x80);
    
    interrupts.Activate();
    
    while(1)
    {
    
    }
}
