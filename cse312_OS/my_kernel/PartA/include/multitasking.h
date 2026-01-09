 
#ifndef __MYOS__MULTITASKING_H
#define __MYOS__MULTITASKING_H

#include <common/types.h>
#include <gdt.h>

namespace myos
{
    enum TaskState {READY, WAITING, FINISHED};

    namespace hardwarecommunication{

        class InterruptHandler;

    }
    /*
    eax: Used for arithmetic and logic operations.
    ebx (Base register): base pointer for memory addressing.
    ecx (Count register):loop counter. Also used for passing function arguments.
    edx (Data register):Often used for I/O operations and arithmetic.
    esi (Source Index): source index for string operations. also as a general-purpose register.
    edi (Destination Index): Used as a destination index for string operations.
    ebp (Base Pointer): base pointer for stack frames. Points to the base of the current stack frame.
    error:to store error codes in certain CPU operations or exceptions.
    eip (Instruction Pointer): Holds the memory address of the next instruction to be executed.
        Determines the flow of control in program execution.
    cs (Code Segment): Stores the segment selector for the code segment.
        Determines the memory segment containing the current instruction.
    eflags (Flags register): Contains status flags that reflect the outcome of arithmetic/logic operations and control flow conditions.
        Flags include carry, zero, sign, overflow, and others.
    esp (Stack Pointer): Points to the top of the stack.
        Used for pushing and popping data onto and from the stack.
    ss (Stack Segment): Stores the segment selector for the stack segment.
        Determines the memory segment containing the stack data.
*/
    struct CPUState
    {
        common::uint32_t eax;
        common::uint32_t ebx;
        common::uint32_t ecx;
        common::uint32_t edx;

        common::uint32_t esi;
        common::uint32_t edi;
        common::uint32_t ebp;

        /*
        common::uint32_t gs;
        common::uint32_t fs;
        common::uint32_t es;
        common::uint32_t ds;
        */
        common::uint32_t error;

        common::uint32_t eip;
        common::uint32_t cs;
        common::uint32_t eflags;
        common::uint32_t esp;
        common::uint32_t ss;        
    } __attribute__((packed));
    
    class Task
    {
        friend class TaskManager;
        private:
            static common::uint32_t pIdCounter;
            // 4 KiB stack memory space allocated for each task object
            common::uint8_t stack[4096]; 
            common::uint32_t pId=0; //set process id as zero if new task object created using constructor
            common::uint32_t pPid=0;// set parent id as zero as well
            TaskState taskState;
            common::uint32_t waitPid;
            CPUState* cpustate;
        public:
            static const int procStackSize = 4096;

            Task(GlobalDescriptorTable *gdt, void entrypoint());
            Task();
            common::uint32_t getId();
            ~Task();
    };
    
    
    
    class TaskManager
    {
        friend class hardwarecommunication::InterruptHandler;
        private:
            Task tasks[256];
            int totTaskCount; // total count of processes being managed by task manager
            int currentTask; // currently running process' index number
            GlobalDescriptorTable *gdt=nullptr;
            int getIndex(common::uint32_t pid);
        protected:
            //added following functions to be used while handling system calls fork,exec,waitpid ect
            common::uint32_t AddTask(void entrypoint());
            common::uint32_t ExecTask(void entrypoint());
            common::uint32_t GetPId();
            common::uint32_t ForkTask(CPUState* cpustate);
            bool ExitCurrentTask();
            bool WaitTask(common::uint32_t pid);
        public:
            void PrintProcessTable();

            TaskManager(GlobalDescriptorTable *gdt);
            ~TaskManager();
            bool AddTask(Task *task);
            CPUState* robinScheduler(CPUState* cpustate);
    };
    
    
    
}


#endif

