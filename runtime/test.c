#include "yu.h"
#include <setjmp.h>
#include <stdio.h>

// This assumes sigsetjmp, siglongjmp uses less than 128 bytes of stack.

static sigjmp_buf restore;
static size_t value;

void g(sigjmp_buf env) {
  int j;
  size_t x, y;

  j = sigsetjmp(restore, 0);
  if (!j) {
    siglongjmp(env, 1);
  } else {
    x = value;
  }

  j = sigsetjmp(restore, 0);
  if (!j) {
    siglongjmp(env, 1);
  } else {
    y = value;
  }

  value = x + y;
  siglongjmp(env, 2);
}

static struct yur_Stack_seg_pair switch_bounds(
    struct yur_Stack_seg *begin, void *end) {
  struct yur_Stack_seg *prev_begin;
  void *prev_end;
  asm ("movq %" yur_STRINGIFY(CURR_SEG_BEGIN) ", %[prev_begin]\n\t"
       "movq %" yur_STRINGIFY(CURR_SEG_END) ", %[prev_end]\n\t"
       "movq %[begin], %" yur_STRINGIFY(CURR_SEG_BEGIN) "\n\t"
       "movq %[end], %" yur_STRINGIFY(CURR_SEG_END)
      : [prev_begin] "=r" (prev_begin), [prev_end] "=r" (prev_end)
      : [begin] "r" (begin), [end] "r" (end));
  return (struct yur_Stack_seg_pair) {prev_begin, prev_end};
}

static struct yur_Stack_seg_pair get_curr_bounds() {
  struct yur_Stack_seg *begin;
  void *end;
  asm ("movq %" yur_STRINGIFY(CURR_SEG_BEGIN) ", %[begin]\n\t"
       "movq %" yur_STRINGIFY(CURR_SEG_END) ", %[end]"
      : [begin] "=r" (begin), [end] "=r" (end));
  return (struct yur_Stack_seg_pair) {begin, end};
}

static size_t handle(
    struct yur_Stack_seg_pair prev_bounds, sigjmp_buf env) {
  struct yur_Stack_seg_pair cont_bounds;
  int j;
  do {
    cont_bounds = switch_bounds(prev_bounds.begin, prev_bounds.end);
    // This applies the continuation to 5:
    value = 5;
    j = sigsetjmp(env, 0);
    if (!j) {
      prev_bounds = switch_bounds(cont_bounds.begin, cont_bounds.end);
      siglongjmp(restore, 1);
    }
  } while (j == 1);
  cont_bounds = switch_bounds(prev_bounds.begin, prev_bounds.end);
  yur_delete_all_stack_segs(cont_bounds.begin, cont_bounds.end);
  return value;
}

yur_SYSTEM_SWITCH_DEF(, void, print_result, (size_t result)) {
  printf("result = %lu\n", result);
}

static void f() {
  size_t ret;
  sigjmp_buf env;
  struct yur_Stack_seg_pair bounds = get_curr_bounds();
  int j = sigsetjmp(env, 0);
  if (j == 0) {
    ret = yur_jmp(g, env);
  } else if (j == 1) {
    ret = handle(bounds, env);
  } else {
    bounds = switch_bounds(bounds.begin, bounds.end);
    yur_delete_all_stack_segs(bounds.begin, bounds.end);
    // Also delete env.
    ret = value;
  }
  print_result(ret);
}

struct yur_Ref *yu_main(struct yur_Ref *) {
  f();
  return &yur_unit;
}
