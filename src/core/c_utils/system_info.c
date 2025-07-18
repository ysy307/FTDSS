#include <stdio.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/utsname.h>
#endif

// OS名を返す（静的バッファへのポインタ）
const char* system_info_get_os(void)
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
const char* system_info_get_cpu_architecture(void)
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
