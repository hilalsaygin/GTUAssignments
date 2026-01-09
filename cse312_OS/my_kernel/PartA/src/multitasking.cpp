
#include <multitasking.h>

using namespace myos;
using namespace myos::common;

common::uint32_t Task::pIdCounter=1;

void printf(char* str);
void printNum(int num);

Task::Task()
{
    taskState;
    pPid=0;
    waitPid=0;
    cpustate = (CPUState*)(stack + Task::procStackSize - sizeof(CPUState));
    cpustate -> eax = 0;
    cpustate -> ebx = 0;
    cpustate -> ecx = 0;
    cpustate -> edx = 0;

    cpustate -> esi = 0;
    cpustate -> edi = 0;
    cpustate -> ebp = 0;
    cpustate -> eip = 0;
    cpustate -> cs = 0;
    cpustate -> eflags = 0x202;
    
}

Task::Task(GlobalDescriptorTable *gdt, void entrypoint())
{
    

    cpustate = (CPUState*)(stack + Task::procStackSize - sizeof(CPUState));
    cpustate -> eax = 0;
    cpustate -> ebx = 0;
    cpustate -> ecx = 0;
    cpustate -> edx = 0;

    cpustate -> esi = 0;
    cpustate -> edi = 0;
    cpustate -> ebp = 0;

    cpustate -> eip = (uint32_t)entrypoint;
    cpustate -> cs = gdt->CodeSegmentSelector();
    cpustate -> eflags = 0x202;
    //add the attributes that will be used for implementing fork 
    // waitPid for multiprocessing, used storing id of the process that the current process is waiting
    taskState = FINISHED;
    waitPid=0;
    pPid=0;
    pId=pIdCounter++;
    
}

common::uint32_t Task::getId(){ //return the id of the task
    return pId;
}
Task::~Task()
{
}
        
TaskManager::TaskManager(GlobalDescriptorTable *gdtImport)
{
    gdt=gdtImport;
    totTaskCount = 0;
    currentTask = -1;
}

TaskManager::~TaskManager()
{
}
// fork request handler in the task manager class
// generates a new child process. 
//copies all info of the current task as info of the new task
//takes 'cpustate' as parameter which is the esp(stack pointer current place) is passed
common::uint32_t TaskManager::ForkTask(CPUState* cpustate)
{
    //if the maximum number of tasks has been reached, no new task can be created. return 0
    if(totTaskCount >= 256)
        return 0;
    // Set the state of the new task as READY
    tasks[totTaskCount].taskState=READY;
    // Assign the parent process id as the the new task process id
    tasks[totTaskCount].pPid=tasks[currentTask].pId;
    //increment the new tasks process id
    tasks[totTaskCount].pId=Task::pIdCounter++;
    // Copy the stack of current task into the stack of newly generated task
    for (int i = 0; i < sizeof(tasks[currentTask].stack); i++)
    {
        tasks[totTaskCount].stack[i]=tasks[currentTask].stack[i];
    }
    // Set the CPU state of the new task from the staring point of the stack 
    tasks[totTaskCount].cpustate = (CPUState*)(tasks[totTaskCount].stack + Task::procStackSize - sizeof(CPUState));
    
    //find where the address of parent' stack pointer is pointing to 
    //calculate the offset between the current task's cpustate pointer and parameter (parent's esp) 
    common::uint32_t currentTaskOffset=((common::uint32_t)tasks[currentTask].cpustate) - ((common::uint32_t)cpustate);
    
    // Adjust the cpu state pointer of the new task considering the current task cpustate
    tasks[totTaskCount].cpustate=(CPUState*)(((common::uint32_t)tasks[totTaskCount].cpustate) - currentTaskOffset);
    tasks[totTaskCount].cpustate->ecx = 0; //indicates that this process is child created by fork
    totTaskCount++; //increment the total number of the task in the task manager
    return tasks[totTaskCount-1].pId; //return the process id of the newly created child process to be stores parent's ecx register
}
//adds a new task to the task manager’s list of tasks. It returns a boolean indicating success or failure.
bool TaskManager::AddTask(Task *task)
{
    if(totTaskCount >= 256)
        return false;

    tasks[totTaskCount].taskState=READY;
    tasks[totTaskCount].pId=task->pId;
    tasks[totTaskCount].cpustate = (CPUState*)(tasks[totTaskCount].stack + Task::procStackSize - sizeof(CPUState));
    
    tasks[totTaskCount].cpustate -> eax = task->cpustate->eax;
    tasks[totTaskCount].cpustate -> ebx = task->cpustate->ebx;
    tasks[totTaskCount].cpustate -> ecx = task->cpustate->ecx;
    tasks[totTaskCount].cpustate -> edx = task->cpustate->edx;

    tasks[totTaskCount].cpustate -> esi = task->cpustate->esi;
    tasks[totTaskCount].cpustate -> edi = task->cpustate->edi;
    tasks[totTaskCount].cpustate -> ebp = task->cpustate->ebp;
    
    tasks[totTaskCount].cpustate -> eip = task->cpustate->eip;
    tasks[totTaskCount].cpustate -> cs = task->cpustate->cs;
    
    tasks[totTaskCount].cpustate -> eflags = task->cpustate->eflags;
    totTaskCount++;
    // PrintProcessTable();
    return true;
}
// Adds a new task with the given entry point and returns its PID.
common::uint32_t TaskManager::AddTask(void entrypoint())
{
    tasks[totTaskCount].taskState=READY;
    tasks[totTaskCount].pId=Task::pIdCounter++;
    tasks[totTaskCount].cpustate = (CPUState*)(tasks[totTaskCount].stack + Task::procStackSize - sizeof(CPUState));
    
    tasks[totTaskCount].cpustate -> eax = 0;
    tasks[totTaskCount].cpustate -> ebx = 0;
    tasks[totTaskCount].cpustate -> ecx = 0;
    tasks[totTaskCount].cpustate -> edx = 0;

    tasks[totTaskCount].cpustate -> esi = 0;
    tasks[totTaskCount].cpustate -> edi = 0;
    tasks[totTaskCount].cpustate -> ebp = 0;
    
    tasks[totTaskCount].cpustate -> eip = (uint32_t)entrypoint;
    tasks[totTaskCount].cpustate -> cs = gdt->CodeSegmentSelector();
    
    tasks[totTaskCount].cpustate -> eflags = 0x202;
    totTaskCount++;
    return tasks[totTaskCount-1].pId;
}

common::uint32_t TaskManager::GetPId(){
    return tasks[currentTask].pId;
}

common::uint32_t TaskManager::ExecTask(void entrypoint())
{
    tasks[currentTask].taskState=READY;
    tasks[currentTask].cpustate = (CPUState*)(tasks[currentTask].stack + Task::procStackSize - sizeof(CPUState));
    
    tasks[currentTask].cpustate -> eax = 0;
    tasks[currentTask].cpustate -> ebx = 0;
    tasks[currentTask].cpustate -> ecx = tasks[currentTask].pId;
    tasks[currentTask].cpustate -> edx = 0;

    tasks[currentTask].cpustate -> esi = 0;
    tasks[currentTask].cpustate -> edi = 0;
    tasks[currentTask].cpustate -> ebp = 0;
    
    tasks[currentTask].cpustate -> eip = (uint32_t)entrypoint;
    tasks[currentTask].cpustate -> cs = gdt->CodeSegmentSelector();
    tasks[currentTask].cpustate -> eflags = 0x202;
    return (uint32_t)tasks[currentTask].cpustate;
}

bool TaskManager::ExitCurrentTask(){
    tasks[currentTask].taskState=FINISHED;
    // PrintProcessTable();
    return true;
}

int TaskManager::getIndex(common::uint32_t pid){
    int index=-1;
    for (int i = 0; i < totTaskCount; i++)
    {
        if(tasks[i].pId==pid){
            index=i;
            break;
        }
    }
    return index;
}

/**
 * The function `WaitTask` in the `TaskManager` class in C++ waits for a specified task identified by
 * its process ID while preventing self-waiting.
 * 
 * @param esp The `esp` parameter in the `TaskManager::WaitTask` function is a pointer to the top of
 * the stack at the time of the interrupt. It is used to access the CPU state and extract the process
 * ID (pid) from the CPU state structure.
 * 
 * @return The function `TaskManager::WaitTask` returns a boolean value - `true` if the task was
 * successfully set to a waiting state, and `false` if any of the conditions for setting the task to
 * waiting state were not met.
 */
bool TaskManager::WaitTask(common::uint32_t esp){
    CPUState *cpustate=(CPUState*)esp;
    common::uint32_t pid=cpustate->ebx;

    if(tasks[currentTask].pId==pid || pid==0) // prevention self waiting
        return false;

    int index=getIndex(pid);
    if(index==-1)
        return false;

    if(totTaskCount<=index || tasks[index].taskState==FINISHED)
        return false;
        
    tasks[currentTask].cpustate=cpustate;
    tasks[currentTask].waitPid=pid;
    tasks[currentTask].taskState=WAITING;
    return true;
}

/**
 * The function `PrintProcessTable` iterates through the task list and prints the process ID and state
 * of each task.
 */
void TaskManager::PrintProcessTable(){

    for (int i = 0; i < totTaskCount; i++)
    {
        printf(" P");
        printNum(tasks[i].pId);
        printf("-Stat:");
        if(tasks[i].taskState==TaskState::READY){
            if(i==currentTask)
                printf("RUNNING ");
            else
                printf("READY ");
        }else if(tasks[i].taskState==TaskState::WAITING){
            printf("WAITING ");
        }else if(tasks[i].taskState==TaskState::FINISHED){
            printf("FINISHED ");
        }
    }
    printf("--\n");
    for(int i = 0; i<1500000; i++){

    }
}

/*
  It's being called by interrupt handler
  implement Round-Robin Scheduling algorithm
*/ 
CPUState* TaskManager::robinScheduler(CPUState* cpustate)
{
    //if no task in the task manager retunr zero
    if(totTaskCount <= 0)
        return cpustate;
    // if there is a currnt task running, save the current task state   
    if(currentTask >= 0)
        tasks[currentTask].cpustate = cpustate;
    //calculate the index of the next task. 
    //this approach ensures the tasks are scheduled in a perspective of Round-Robin Scheduling. 
    //the index is calcualated considering the array is implemented in a circular way. 
    int to_be_scheduled = (currentTask+1)%totTaskCount;
    //iterate over the tasks array, till finding a task that is ready
    // if task state not ready enter
    while (tasks[to_be_scheduled].taskState!=READY)
    {
        // Check if the process is waiting for another task to finish. 
        //If the current is waiting for another, its process id is stored in waitPid attribute
        //if the task, its id stored at waitpid, is finished then enter
        if(tasks[to_be_scheduled].taskState==WAITING && tasks[to_be_scheduled].waitPid>0){

            int waitIdx=0;
            //get the index of the task where its process Id is stored in waitpid attribute of to_be_scheduled
            waitIdx=getIndex(tasks[to_be_scheduled].waitPid);
            //check if the process in the waitpid is waiting for another task.
            //if it 
            if(waitIdx>-1 && tasks[waitIdx].taskState!=WAITING){
                //(since the process, which indicated in waitPid, is finished now the current task is ready to execute)
                // if process in waitPid is finished, update the status of current task from WAITING to READY

                if (tasks[waitIdx].taskState==FINISHED)
                {
                    tasks[to_be_scheduled].waitPid=0;
                    tasks[to_be_scheduled].taskState=READY;
                    continue;
                // If the task being waited for is ready, schedule it next

                }else if (tasks[waitIdx].taskState==READY)
                {
                    to_be_scheduled=waitIdx;
                    continue; // Continue to schedule the waiting task
                }
                
            }
            
        }
        // Move to the next task in a circular manner

        to_be_scheduled= (++to_be_scheduled) % totTaskCount;

    }
    // Update the current task to be the one found for scheduling
    currentTask=to_be_scheduled;
    PrintProcessTable();
    // Return the CPU state of the scheduled task
    return tasks[currentTask].cpustate;
}

