#ifndef YU_CMALLOC_H
#define YU_CMALLOC_H

#include "yu.h"

yur_SYSTEM_SWITCH_DECL(, void *, yur_cmalloc, (size_t));

yur_SYSTEM_SWITCH_DECL(, void, yur_cfree, (void *));

#endif // YU_CMALLOC_H
