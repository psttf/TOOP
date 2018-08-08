#include <process.h>

int main(void)
{
    execlp("java",
        "-jar",
        "sigmac.jar");
    return 0;
}