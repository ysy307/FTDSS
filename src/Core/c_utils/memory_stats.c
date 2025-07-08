#include <stdint.h>
#include <stdio.h>

#ifdef _WIN32
#include <windows.h>
#include <psapi.h> // GetProcessMemoryInfo に必要
#else
#include <unistd.h> // sysconf に必要
#endif

/**
 * @brief 現在のプロセスの物理メモリ使用量（RSS）をキロバイト単位で取得します。
 *
 * @return int64_t 成功した場合はRSS(kB)。失敗した場合は-1。
 */
int64_t memory_stats_get_rss_kb(void)
{
#ifdef _WIN32
    PROCESS_MEMORY_COUNTERS info;
    // GetProcessMemoryInfoは成功/失敗を返すので、返り値をチェックします。
    if (!GetProcessMemoryInfo(GetCurrentProcess(), &info, sizeof(info)))
    {
        return -1;
    }
    return (int64_t)(info.WorkingSetSize / 1024);
#else
    // /proc/self/statm ファイルを開きます。
    // このファイルの2番目の数値がRSS（Resident Set Size）をページ単位で示します。
    FILE *f = fopen("/proc/self/statm", "r");
    if (!f)
    {
        return -1;
    }

    // 1番目の値(VSZ)を読み飛ばし、2番目の値(RSS)を取得します。
    unsigned long dummy_vsz, resident_pages;
    if (fscanf(f, "%lu %lu", &dummy_vsz, &resident_pages) != 2)
    {
        fclose(f);
        return -1;
    }
    fclose(f);

    // ページのサイズ（バイト単位）を取得します。
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0)
    {
        return -1;
    }

    // ページ数 × ページサイズ で総バイト数を計算し、キロバイトに変換します。
    return (int64_t)resident_pages * page_size / 1024;
#endif
}