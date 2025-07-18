#define _POSIX_C_SOURCE 200809L

#include <signal.h>
#include <stdint.h> // uintptr_t のために追加
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// フラグを volatile にしてシグナルセーフに
static volatile sig_atomic_t g_interrupted_flag = 0;

// SIGINT（Ctrl+C）ハンドラ
static void sigint_handler(int sig)
{
    (void)sig; // 未使用引数の警告を抑制
    g_interrupted_flag = 1;
}

// SIGSEGV（セグメンテーションフォルト）ハンドラ
static void sigsegv_handler(int sig, siginfo_t *info, void *ucontext)
{
    (void)sig;
    (void)ucontext;

    // 非同期シグナルセーフな関数のみを使用
    char msg_buffer[256];

    // snprintf は非同期シグナルセーフではないが、ここではデバッグ目的で利用
    // 厳密な安全性よりデバッグの容易さを優先する場合が多い
    int len = snprintf(msg_buffer, sizeof(msg_buffer),
                       "\n--- Caught SIGSEGV (Segmentation Fault) ---\n"
                       "    Faulting address: 0x%lx\n"
                       "-------------------------------------------\n",
                       (uintptr_t)info->si_addr);

    write(STDERR_FILENO, msg_buffer, len);

    // セグフォ後は安全のため即終了
    _exit(139); // 128 + SIGSEGV(11)
}

// Fortran から呼ばれる初期化関数
void setup_signal_handler(void)
{
    struct sigaction sa_int, sa_segv;

    // ----------- SIGINT 設定 -----------
    sigemptyset(&sa_int.sa_mask);
    sa_int.sa_handler = sigint_handler;
    sa_int.sa_flags = SA_RESTART; // システムコールを中断しない
    sigaction(SIGINT, &sa_int, NULL);

    // ----------- SIGSEGV 設定 -----------
    sigemptyset(&sa_segv.sa_mask);
    sa_segv.sa_sigaction = sigsegv_handler;
    sa_segv.sa_flags = SA_SIGINFO; // 拡張情報使用
    sigaction(SIGSEGV, &sa_segv, NULL);
}

// Fortran からシグナルフラグを取得
int get_interrupted_flag(void)
{
    return g_interrupted_flag;
}
