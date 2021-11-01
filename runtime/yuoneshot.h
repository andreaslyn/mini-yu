#ifndef YU_YUONESHOT_H
#define YU_YUONESHOT_H

#include "yu.h"

typedef size_t yur_Jmp_buf[8];

typedef void (*yur_Oneshot_Wrap)(yur_Ref *prompt, yur_Ref *callback);

yur_NORETURN void
yur_oneshot_jmp(yur_Oneshot_Wrap jmp, yur_Ref *prompt, yur_Ref *callback);

__attribute__((__returns_twice__)) int yur_setjmp(yur_Jmp_buf);

yur_NORETURN void yur_longjmp(yur_Jmp_buf, int);

#endif // YU_YUONESHOT_H
