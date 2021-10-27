#include "yucmalloc.h"
#include <stdlib.h>

yur_SYSTEM_SWITCH_DEF(, void *, yur_cmalloc, (size_t s)) {
  return malloc(s);
}

yur_SYSTEM_SWITCH_DEF(, void, yur_cfree, (void *p)) {
  free(p);
}
