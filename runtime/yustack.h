#include <stddef.h>
#include <stdbool.h>

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

void yur_set_stack_overlap_size(size_t);
void yur_set_stack_segment_size(size_t);
void yur_enable_stack_red_zone(bool);

struct yur_Stack_seg_pair
yur_new_stack_seg(
    size_t copy_size, // Amount of stack to copy from prev_sp
    struct yur_Stack_seg *prev_seg_begin,
    void *prev_seg_end,
    void *prev_sp,
    size_t frame_size, // Size of caller stack frame.
    size_t prev_frame_size); // Size of caller's caller stack frame.

struct yur_Stack_seg_pair
yur_initial_stack_seg(size_t frame_size);

void
yur_delete_stack_seg(void *prev_seg_end);

void
yur_delete_all_stack_segs(struct yur_Stack_seg *seg_begin, void *seg_end);

typedef void *(*yur_Run)(void *);

void *yur_run(yur_Run, void *);
