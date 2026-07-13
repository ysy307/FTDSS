#ifndef _WIN32
#define _POSIX_C_SOURCE 200809L
#endif

#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#endif

static volatile sig_atomic_t g_interrupted_flag = 0;

static void sigint_handler(int sig)
{
    (void)sig;
    g_interrupted_flag = 1;
}

#ifdef _WIN32
static void sigsegv_handler(int sig)
{
    (void)sig;
    fputs("\n--- Caught SIGSEGV (Segmentation Fault) ---\n", stderr);
    _Exit(139);
}
#else
static void sigsegv_handler(int sig, siginfo_t *info, void *ucontext)
{
    (void)sig;
    (void)ucontext;

    char msg_buffer[256];

    // snprintf is used here to preserve the fault address in diagnostics.
    int len = snprintf(msg_buffer, sizeof(msg_buffer),
                       "\n--- Caught SIGSEGV (Segmentation Fault) ---\n"
                       "    Faulting address: 0x%lx\n"
                       "-------------------------------------------\n",
                       (uintptr_t)info->si_addr);

    write(STDERR_FILENO, msg_buffer, len);

    _exit(139);
}
#endif

void setup_signal_handler(void)
{
#ifdef _WIN32
    signal(SIGINT, sigint_handler);
    signal(SIGSEGV, sigsegv_handler);
#else
    struct sigaction sa_int, sa_segv;

    sigemptyset(&sa_int.sa_mask);
    sa_int.sa_handler = sigint_handler;
    sa_int.sa_flags = SA_RESTART;
    sigaction(SIGINT, &sa_int, NULL);

    sigemptyset(&sa_segv.sa_mask);
    sa_segv.sa_sigaction = sigsegv_handler;
    sa_segv.sa_flags = SA_SIGINFO;
    sigaction(SIGSEGV, &sa_segv, NULL);
#endif
}

int get_interrupted_flag(void)
{
    return g_interrupted_flag;
}
