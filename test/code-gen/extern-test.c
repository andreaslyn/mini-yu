#include "yu.h"
#include <stdio.h>

yur_SYSTEM_SWITCH_DEF(, static void, print_hello, ()) {
  printf("hello extern world\n");
}

yur_Ref *yu__ex(yur_Ref *b) {
  yur_Ref *ret;
  print_hello();
  if (yur_ALOAD(b->tag) == 0)
    ret = yur_build(0, 1);
  else
    ret = yur_build(0, 0);
  yur_unref(b);
  return ret;
}

yur_Ref *yu_len(yur_Ref *t, yur_Ref *A) {
  yur_Ref *ret;
  if (yur_ALOAD(t->tag) == 0) {
    ret = yur_build(0, 0);
  } else {
    yur_Ref *c = t->fields[1];
    yur_inc(c);
    yur_Ref *x = yu_len(c, A);
    ret = yur_build(1, 1);
    ret->fields[0] = x;
  }
  yur_unref(t);
  return ret;
}
