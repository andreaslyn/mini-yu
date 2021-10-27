#include "yu.h"
#include <string.h>
#include <mimalloc.h>
#include <errno.h>

extern volatile size_t yur_stack_overlap_size;
extern volatile size_t yur_reserved_stack_size;
extern volatile size_t yur_maximal_stack_segment_size;
extern volatile size_t yur_initial_stack_segment_size;

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static size_t,
    get_current_segment_size, ()) {
  size_t begin, end;
  asm("movq %%fs:0x80, %[begin]\n\t"
      "movq %%fs:0x70, %[end]"
      : [begin] "=r" (begin), [end] "=r" (end));
  return
    begin - end
    + yur_reserved_stack_size
    + sizeof(struct yur_Stack_seg);
}

// EXTRA_SEG_RESERVE is indicating the minimal amount of reserved
// stack segment space. Currently we need 16 bytes, to have room
// for two return addresses on the stack, and we reserve 224 bytes
// for a more efficient prologue when stack frame is <= 224 bytes.
#define EXTRA_SEG_RESERVE (16 + 224)

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static size_t,
    byte16_align, (size_t x)) {
  return (x + 15) & ~15;
}

yur_SYSTEM_DEF(void, yur_set_stack_overlap_size, (size_t size)) {
  yur_stack_overlap_size = size;
}

yur_SYSTEM_DEF(void, yur_enable_stack_red_zone, (bool b)) {
  // On top of to red zone, we need to reserve EXTRA_SEG_RESERVE bytes.
  if (b)
    yur_reserved_stack_size = 128 + EXTRA_SEG_RESERVE;
  else
    yur_reserved_stack_size = EXTRA_SEG_RESERVE;
}

yur_SYSTEM_DEF(void, yur_set_initial_stack_segment_size, (size_t size)) {
  if (size & 15)
    yur_panic_s("initial stack segment size should be 16 byte aligned");
  yur_initial_stack_segment_size = size;
}

yur_SYSTEM_DEF(void, yur_set_maximal_stack_segment_size, (size_t size)) {
  if (size & 15)
    yur_panic_s("maximal stack segment size should be 16 byte aligned");
  yur_maximal_stack_segment_size = size;
}

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static size_t, get_alloc_size,
    (size_t copy_size, size_t frame_size)) {
  size_t next_seg_size = get_current_segment_size_s() << 1;
  if (next_seg_size > yur_maximal_stack_segment_size)
    next_seg_size = yur_maximal_stack_segment_size;

  size_t min_size = copy_size
    + frame_size
    + yur_reserved_stack_size
    + sizeof(struct yur_Stack_seg);
  min_size = byte16_align_s(min_size);

  return min_size >= next_seg_size
    ?  min_size
    : next_seg_size;
}

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static void *, alloc, (size_t seg_size)) {
  void *m = mi_malloc_aligned(seg_size, 16);
  if (!m)
    yur_panic_s("stack allocation error");
  return m;
}

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static struct yur_Stack_seg *,
    get_seg_begin, (void *segp, size_t seg_size)) {
  return (struct yur_Stack_seg *)
    ((char *) segp + seg_size - sizeof(struct yur_Stack_seg));
}

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static struct yur_Stack_seg *,
    get_seg_end, (void *segp)) {
  return (void *) ((char *) segp + yur_reserved_stack_size);
}

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static size_t,
    do_reflect_old_stack_part,
      (size_t copy_size,
       size_t begin,
       size_t prev_sp,
       size_t prev_frame_size)) {
  size_t p = begin - copy_size;
  memcpy((void *) p, (void *) prev_sp, copy_size);
  size_t total = prev_frame_size - 8;
  size_t bp = prev_sp + total;
  while (total < copy_size - 8) {
    size_t nbp = * (size_t *) bp;
    size_t ntotal = total + nbp - bp;
    * (size_t *) (p + total) = p + ntotal;
    total = ntotal;
    bp = nbp;
  }
  * (size_t *) (p + total) = begin;
  return * (size_t *) bp;
}

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static void *,
    reflect_old_stack_part,
      (size_t copy_size,
       struct yur_Stack_seg *begin,
       void *prev_sp,
       size_t prev_frame_size)) {
  return (void *) do_reflect_old_stack_part_s(
      copy_size, (size_t) begin, (size_t) prev_sp, prev_frame_size);
}

yur_SYSTEM_DEF(struct yur_Stack_seg_pair, yur_new_stack_seg,
    (size_t copy_size,
     struct yur_Stack_seg *prev_seg_begin,
     void *prev_seg_end,
     void *prev_sp,
     size_t frame_size,
     size_t prev_frame_size)) {
  size_t s = get_alloc_size_s(copy_size, frame_size);

  void *p = alloc_s(s);

  struct yur_Stack_seg *begin = get_seg_begin_s(p, s);

  void *prev_bp = reflect_old_stack_part_s(
      copy_size, begin, prev_sp, prev_frame_size);

  // Add the stack amount to copy from the old stack pointer prev_sp,
  // since that is where it is expected to point when it is needed
  // in __yu_lessstack.
  begin->prev_sp = (void *) ((char *) prev_sp + copy_size);
  begin->prev_seg_end = prev_seg_end;
  begin->prev_seg_begin = prev_seg_begin;
  begin->prev_bp = prev_bp;

  void *end = get_seg_end_s(p);

  return (struct yur_Stack_seg_pair) { begin, end };
}

yur_SYSTEM_DEF(yur_ALWAYS_INLINE static size_t, get_initial_alloc_size,
    (size_t frame_size)) {
  size_t min_size =
    frame_size
    + yur_reserved_stack_size
    + sizeof(struct yur_Stack_seg);
  min_size = byte16_align_s(min_size);
  return min_size >= yur_initial_stack_segment_size
    ?  min_size
    : yur_initial_stack_segment_size;
}

yur_SYSTEM_DEF(struct yur_Stack_seg_pair,
    yur_initial_stack_seg, (size_t frame_size)) {
  size_t s = get_initial_alloc_size_s(frame_size);
  void *p = alloc_s(s);
  struct yur_Stack_seg *begin = get_seg_begin_s(p, s);
  void *end = get_seg_end_s(p);
  bzero(begin, YUR_STACK_SEG_UNRESERVED_SIZE);
  return (struct yur_Stack_seg_pair) { begin, end };
}

yur_SYSTEM_DEF(extern yur_ALWAYS_INLINE void,
    yur_delete_stack_seg, (void *prev_seg_end)) {
  mi_free((char *) prev_seg_end - yur_reserved_stack_size);
}

yur_SYSTEM_DEF(void, yur_delete_all_stack_segs,
    (struct yur_Stack_seg *seg_begin, void *seg_end)) {
  if (seg_begin->prev_seg_begin)
    yur_delete_all_stack_segs_s(
        seg_begin->prev_seg_begin, seg_begin->prev_seg_end);
  yur_delete_stack_seg_s(seg_end);
}
