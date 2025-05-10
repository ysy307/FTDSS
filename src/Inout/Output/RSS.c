#include <stdint.h>

#if defined(_WIN32)

#include <windows.h>
#include <psapi.h>

int64_t get_rss_kb()
{
    PROCESS_MEMORY_COUNTERS info;
    GetProcessMemoryInfo(GetCurrentProcess(), &info, sizeof(info));
    return (int64_t)(info.WorkingSetSize / 1024); // bytes to kB
}

#elif defined(__APPLE__) && defined(__MACH__)

#include <mach/mach.h>

int64_t get_rss_kb()
{
    mach_task_basic_info_data_t info;
    mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
    task_t task = mach_task_self();

    if (task_info(task, MACH_TASK_BASIC_INFO, (task_info_t)&info, &count) != KERN_SUCCESS)
        return -1;

    return (int64_t)(info.resident_size / 1024); // bytes to kB
}

#else

#include <unistd.h>
#include <stdio.h>

int64_t get_rss_kb()
{
    long pages = 0;
    FILE *f = fopen("/proc/self/statm", "r");
    if (f == NULL)
        return -1;
    if (fscanf(f, "%ld", &pages) != 1)
    {
        fclose(f);
        return -1;
    }
    fclose(f);
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0)
        return -1;
    return (int64_t)(pages * page_size / 1024); // bytes to kB
}

#endif
