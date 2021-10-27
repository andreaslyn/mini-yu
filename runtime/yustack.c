#include "yu.h"
#include <string.h>
#include <mimalloc.h>
#include <errno.h>

extern volatile size_t yur_stack_overlap_size;
extern volatile size_t yur_stack_segment_size;
extern volatile size_t yur_reserved_stack_size;

// EXTRA_SEG_RESERVE is indicating the minimal amount of reserved
// stack segment space. Currently we need 16 bytes, to have room
// for two return addresses on the stack while obtaining more stack.
#define EXTRA_SEG_RESERVE 16

yur_NO_SPLIT_STACK yur_ALWAYS_INLINE static size_t
byte16_align(size_t x)
{
  return (x + 15) & ~15;
}

yur_NO_SPLIT_STACK void
yur_set_stack_overlap_size(size_t size)
{
  yur_stack_overlap_size = size;
}

yur_NO_SPLIT_STACK void
yur_set_stack_segment_size(size_t size)
{
  if (size & 15)
    yur_SYSTEM_SWITCH_yur_panic("stack segment size should be 16 byte aligned");
  yur_stack_segment_size = size;
}

yur_NO_SPLIT_STACK void
yur_enable_stack_red_zone(bool b)
{
  // On top of to red zone, we need to reserve EXTRA_SEG_RESERVE bytes.
  if (b)
    yur_reserved_stack_size = 128 + EXTRA_SEG_RESERVE;
  else
    yur_reserved_stack_size = EXTRA_SEG_RESERVE;
}

yur_NO_SPLIT_STACK yur_ALWAYS_INLINE static size_t
get_alloc_size(size_t copy_size, size_t frame_size)
{
  size_t min_size = copy_size
    + frame_size
    + yur_reserved_stack_size
    + sizeof(struct yur_Stack_seg);
  min_size = byte16_align(min_size);
  return min_size >= yur_stack_segment_size
    ?  min_size
    : yur_stack_segment_size;
}

yur_NO_SPLIT_STACK yur_ALWAYS_INLINE static void *
alloc(size_t seg_size)
{
  void *m = mi_malloc_aligned(seg_size, 16);
  if (!m)
    yur_SYSTEM_SWITCH_yur_panic("stack allocation error\n");
  return m;
}

yur_NO_SPLIT_STACK yur_ALWAYS_INLINE static struct yur_Stack_seg *
get_seg_begin(void *segp, size_t seg_size)
{
  return (struct yur_Stack_seg *)
    ((char *) segp + seg_size - sizeof(struct yur_Stack_seg));
}

yur_NO_SPLIT_STACK yur_ALWAYS_INLINE static struct yur_Stack_seg *
get_seg_end(void *segp)
{
  return (void *) ((char *) segp + yur_reserved_stack_size);
}

yur_NO_SPLIT_STACK yur_ALWAYS_INLINE static size_t
do_reflect_old_stack_part(
    size_t copy_size,
    size_t begin,
    size_t prev_sp,
    size_t prev_frame_size)
{
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

yur_NO_SPLIT_STACK yur_ALWAYS_INLINE static void *
reflect_old_stack_part(
    size_t copy_size,
    struct yur_Stack_seg *begin,
    void *prev_sp,
    size_t prev_frame_size)
{
  return (void *) do_reflect_old_stack_part(
      copy_size, (size_t) begin, (size_t) prev_sp, prev_frame_size);
}

yur_NO_SPLIT_STACK struct yur_Stack_seq_end_pair
yur_new_stack_seg(
    size_t copy_size,
    struct yur_Stack_seg *prev_seg_begin,
    void *prev_seg_end,
    void *prev_sp,
    size_t frame_size,
    size_t prev_frame_size)
{
  size_t s = get_alloc_size(copy_size, frame_size);

  void *p = alloc(s);

  struct yur_Stack_seg *begin = get_seg_begin(p, s);

  void *prev_bp = reflect_old_stack_part(
      copy_size, begin, prev_sp, prev_frame_size);

  // Add the stack amount to copy from the old stack pointer prev_sp,
  // since that is where it is expected to point when it is needed
  // in __yu_lessstack.
  begin->prev_sp = (void *) ((char *) prev_sp + copy_size);
  begin->prev_seg_end = prev_seg_end;
  begin->prev_seg_begin = prev_seg_begin;
  begin->prev_bp = prev_bp;

  void *end = get_seg_end(p);

  return (struct yur_Stack_seq_end_pair) { begin, end };
}

yur_NO_SPLIT_STACK struct yur_Stack_seq_end_pair
yur_initial_stack_seg(size_t frame_size)
{
  size_t s = get_alloc_size(0, frame_size);
  void *p = alloc(s);
  struct yur_Stack_seg *begin = get_seg_begin(p, s);
  void *end = get_seg_end(p);
  bzero(begin, sizeof(*begin));
  return (struct yur_Stack_seq_end_pair) { begin, end };
}

yur_NO_SPLIT_STACK yur_ALWAYS_INLINE void
yur_delete_stack_seg(void *prev_seg_end)
{
  mi_free((char *) prev_seg_end - yur_reserved_stack_size);
}

yur_NO_SPLIT_STACK void
yur_delete_all_stack_segs(struct yur_Stack_seg *seg_begin, void *seg_end)
{
  if (seg_begin->prev_seg_begin)
    yur_delete_all_stack_segs(seg_begin->prev_seg_begin, seg_begin->prev_seg_end);
  yur_delete_stack_seg(seg_end);
}
