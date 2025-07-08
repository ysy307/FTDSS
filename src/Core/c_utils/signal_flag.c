#include <signal.h>

// グローバルなvolatile変数でフラグを管理
static volatile int g_interrupted_flag = 0;

// 安全なシグナルハンドラ：フラグを立てるだけ
void sigint_handler(int sig)
{
    g_interrupted_flag = 1;
}

// Fortranから呼び出すセットアップ関数
void setup_signal_handler()
{
    signal(SIGINT, sigint_handler);
}

// Fortranから呼び出すフラグ取得関数
int get_interrupted_flag()
{
    return g_interrupted_flag;
}