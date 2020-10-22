#include "yulist.h"

size_t yur_yulist_csize(yur_Ref *r) {
  size_t s = 0;
  yur_Ref *x = r;
  while (x->tag) {
    ++s;
    x = x->fields[1];
  }
  yur_unref(r);
  return s;
}
