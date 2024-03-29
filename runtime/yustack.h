#ifndef YU_YUSTACK_H
#define YU_YUSTACK_H

#include "yubase.h"
#include <stddef.h>
#include <stdbool.h>

#define yur_SYSTEM_DECL(type, name, args) \
  type name ## _s args

#ifndef yur_DISABLE_SPLIT_STACK

#define yur_SYSTEM_DEF(type, name, args) \
  __attribute__((no_split_stack)) type name ## _s args

#else

#define yur_SYSTEM_DEF(type, name, args) \
  type name ## _s args

#endif

#define yur_SYSTEM_SWITCH_DECL(attributes, type, name, args) \
  type name args; \
  attributes type name ## _s args

#ifndef yur_DISABLE_SPLIT_STACK

#define yur_SYSTEM_SWITCH_DEF(attributes, type, name, args) \
  __attribute__((no_split_stack)) yur_NOINLINE yur_NAKED \
  type name args { \
    asm volatile ( \
      "movq %%r15, -8(%%rsp)\n\t" \
      "movq %%rsp, %%r15\n\t" \
      "movq %" yur_STRINGIFY(SYSTEM_STACK) ", %%rsp\n\t" \
      "call " yur_STRINGIFY(name ## _s) "\n\t" \
      "movq %%r15, %%rsp\n\t" \
      "movq -8(%%rsp), %%r15\n\t" \
      "cmpq %%rbp, %%rsp\n\t" \
      "je   __yu_lessstack\n\t" \
      "ret" : : : ); \
  } \
  __attribute__((no_split_stack)) __attribute__((used)) attributes \
  type name ## _s args

#else

#define yur_SYSTEM_SWITCH_DEF(attributes, type, name, args) \
  type name args; \
  asm ( \
    ".globl " yur_STRINGIFY(name) "\n\t" \
    ".type " yur_STRINGIFY(name) ", @function\n" \
    ".p2align 4\n" \
    yur_STRINGIFY(name) ":\n\t" \
    ".cfi_startproc\n\t" \
    "jmp " yur_STRINGIFY(name ## _s) "\n\t" \
    ".cfi_endproc\n"); \
  __attribute__((used)) attributes \
  type name ## _s args

#endif

struct yur_Stack_seg {
  void *prev_sp;
  struct yur_Stack_seg *prev_seg_begin;
  void *prev_seg_end;
  void *prev_bp;
  void *reserved_word;
};
#define YUR_STACK_SEG_UNRESERVED_SIZE \
  (sizeof(struct yur_Stack_seg) - sizeof(void*))
// | reserved | stack... | prev_sp | prev_begin | prev_end | prev_bp | |
//            ^          ^
//            end        begin

struct yur_Stack_seg_pair {
  struct yur_Stack_seg *begin;
  void *end;
};

yur_SYSTEM_DECL(void, yur_set_stack_overlap_size, (size_t));
yur_SYSTEM_DECL(void, yur_enable_stack_red_zone, (bool));
yur_SYSTEM_DECL(void, yur_set_initial_stack_segment_size, (size_t));
yur_SYSTEM_DECL(void, yur_set_maximal_stack_segment_size, (size_t));

struct yur_Stack_seg_pair yur_get_seg_bounds();

void yur_set_seg_bounds(struct yur_Stack_seg *begin, void *end);

struct yur_Stack_seg_pair
yur_switch_seg_bounds(struct yur_Stack_seg *begin, void *end);

struct yur_Stack_seg_pair
yur_new_stack_seg(
    size_t copy_size, // Amount of stack to copy from prev_sp
    struct yur_Stack_seg *prev_seg_begin,
    void *prev_seg_end,
    void *prev_sp,
    size_t frame_size, // Size of caller stack frame.
    size_t prev_frame_size); // Size of caller's caller stack frame.

yur_SYSTEM_SWITCH_DECL(, struct yur_Stack_seg_pair,
    yur_initial_stack_seg, (size_t frame_size));

yur_SYSTEM_DECL(void, yur_delete_stack_seg, (void *prev_seg_end));

yur_SYSTEM_SWITCH_DECL(, void, yur_delete_all_stack_segs,
    (struct yur_Stack_seg *seg_begin, void *seg_end));

typedef void *(*yur_Run)(void *);

#ifndef yur_DISABLE_SPLIT_STACK

yur_SYSTEM_DECL(void *, yur_run, (yur_Run, void *));

#else

yur_SYSTEM_DEF(yur_ALWAYS_INLINE void *,
    yur_run_s, (yur_Run f, void *x)) {
  return f(x);
}

#endif

#endif // YU_YUSTACK_H
