#include "yucmalloc.h"
#include <stdlib.h>

yur_SYSTEM_SWITCH(void *, yur_cmalloc, (size_t s)) {
  return malloc(s);
}

yur_SYSTEM_SWITCH(void, yur_cfree, (void *p)) {
  free(p);
}
