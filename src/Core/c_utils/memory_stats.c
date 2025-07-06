#include <stdint.h>
#include <stdio.h>

#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
#else
#include <unistd.h>
#endif

// RSS を kB 単位で返す
int64_t memory_stats_get_rss_kb(void)
{
#ifdef _WIN32
    PROCESS_MEMORY_COUNTERS info;
    GetProcessMemoryInfo(GetCurrentProcess(), &info, sizeof(info));
    return (int64_t)(info.WorkingSetSize / 1024);
#else
    long pages = 0;
    FILE *f = fopen("/proc/self/statm", "r");
    if (!f)
        return -1;
    if (fscanf(f, "%ld", &pages) != 1)
    {
        fclose(f);
        return -1;
    }
    fclose(f);
    long ps = sysconf(_SC_PAGESIZE);
    if (ps <= 0)
        return -1;
    return (int64_t)(pages * ps / 1024);
#endif
}