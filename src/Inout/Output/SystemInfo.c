#include <stdint.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
#else
#include <unistd.h>
#include <sys/utsname.h>
#endif

// RSS を kB 単位で返す
int64_t get_rss_kb(void)
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

// OS 名を返す（静的バッファへのポインタ）
const char *C_Get_OS(void)
{
    static char osname[64];
#ifdef _WIN32
    snprintf(osname, sizeof(osname), "Windows");
#elif defined(__APPLE__)
    snprintf(osname, sizeof(osname), "macOS");
#else
    struct utsname u;
    if (uname(&u) == 0)
        snprintf(osname, sizeof(osname), "%s", u.sysname);
    else
        snprintf(osname, sizeof(osname), "Unknown");
#endif
    return osname;
}

// アーキテクチャ名を返す（静的バッファへのポインタ）
const char *C_Get_Architecture(void)
{
    static char arch[64];
#ifdef _WIN32
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    switch (si.wProcessorArchitecture)
    {
    case PROCESSOR_ARCHITECTURE_AMD64:
        snprintf(arch, sizeof(arch), "x86_64");
        break;
    case PROCESSOR_ARCHITECTURE_INTEL:
        snprintf(arch, sizeof(arch), "x86");
        break;
    case PROCESSOR_ARCHITECTURE_ARM64:
        snprintf(arch, sizeof(arch), "arm64");
        break;
    default:
        snprintf(arch, sizeof(arch), "Unknown");
        break;
    }
#else
    struct utsname u;
    if (uname(&u) == 0)
        snprintf(arch, sizeof(arch), "%s", u.machine);
    else
        snprintf(arch, sizeof(arch), "Unknown");
#endif
    return arch;
}
