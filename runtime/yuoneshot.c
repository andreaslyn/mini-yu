#include "yuoneshot.h"

#ifndef yur_DISABLE_SPLIT_STACK

struct yur_Prompt {
  yur_Ref_struct;
  yur_Ref cont;
  yur_Ref *argument;
  yur_Ref *handler;
  struct yur_Stack_seg_pair reset_bounds;
  struct yur_Stack_seg_pair shift_bounds;
  yur_Jmp_buf reset_env;
  yur_Jmp_buf shift_env;
};

// PromptAxiom : Ty -> Ty
yur_Ref *yu_PromptAxiom_doyu_slOneshot_slOneshot(yur_Ref *) {
  return &yur_unit;
}

#define setjmp_with_seg(pt) \
  ({ \
    ((struct yur_Prompt *) (pt))->reset_bounds = yur_get_seg_bounds(); \
    yur_setjmp(((struct yur_Prompt *) (pt))->reset_env); \
  })

static yur_ALWAYS_INLINE yur_Ref *undo_prompt_get_result(yur_Ref *pt) {
  struct yur_Stack_seg_pair reset_bounds =
    ((struct yur_Prompt *) pt)->reset_bounds;
  struct yur_Stack_seg_pair bounds =
    yur_switch_seg_bounds(reset_bounds.begin, reset_bounds.end);
  yur_Ref *ret = ((struct yur_Prompt *) pt)->argument;
  yur_unref(pt);
  yur_delete_all_stack_segs(bounds.begin, bounds.end);
  return ret;
}

static yur_ALWAYS_INLINE yur_Ref *handle_shift(yur_Ref *pt) {
  struct yur_Stack_seg_pair reset_bounds =
    ((struct yur_Prompt *) pt)->reset_bounds;
  ((struct yur_Prompt *) pt)->shift_bounds =
    yur_switch_seg_bounds(reset_bounds.begin, reset_bounds.end);
  yur_Ref *c = &((struct yur_Prompt *) pt)->cont;
  yur_Ref *handler = ((struct yur_Prompt *) pt)->handler;
  yur_Ref *(*h)(yur_Ref *, yur_Ref *) =
    (yur_Ref *(*)(yur_Ref *, yur_Ref *)) handler->tag;
  return h(c, handler);
}

// A ->> B
static yur_Ref *cont_fn(yur_Ref *a, yur_Ref *self) {
  struct yur_Prompt *pt = (struct yur_Prompt *)
    ((size_t) self - yur_OFFSET_OF(struct yur_Prompt, cont));
  struct yur_Stack_seg_pair bounds = pt->shift_bounds;
  // No need to yur_unref(self), since it is marked static and
  // stored as part of struct yur_Prompt (cont).
  pt->argument = a;
  switch (setjmp_with_seg((yur_Ref *) pt)) {
  case 0:
    yur_set_seg_bounds(bounds.begin, bounds.end);
    yur_longjmp(pt->shift_env, 1); // noreturn
  case 1:
    return handle_shift((yur_Ref *) pt);
  default:
    return undo_prompt_get_result((yur_Ref *) pt);
  }
}

static yur_NORETURN void oneshot_wrap(yur_Ref *pt, yur_Ref *self) {
  yur_Ref *ret = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) self->tag)(pt, self);
  ((struct yur_Prompt *) pt)->argument = ret;
  yur_longjmp(((struct yur_Prompt *) pt)->reset_env, 2);
}

// resetAxiom : (A : Ty) & (Prompt A ->> A) ->> A
yur_Ref *yu_resetAxiom_doyu_slOneshot_slOneshot(yur_Ref *f, yur_Ref *A) {
  yur_Ref *pt = yur_malloc(sizeof(struct yur_Prompt));
  yur_init(pt, 0, 0);
  yur_init(&((struct yur_Prompt *) pt)->cont, 0, (size_t) cont_fn);
  // Mark static:
  ((struct yur_Prompt *) pt)->cont.vmt_index = yur_Static_vmt;
  ((struct yur_Prompt *) pt)->cont.count = 0;
  yur_inc(pt);
  switch (setjmp_with_seg(pt)) {
  case 0:
    yur_oneshot_jmp(oneshot_wrap, pt, f); // noreturn
  case 1:
    return handle_shift(pt);
  default:
    return undo_prompt_get_result(pt);
  }
}

// shiftAxiom : (A B : Ty) & ((A ->> B) ->> B) & Prompt B ->> A
yur_Ref *yu_shiftAxiom_doyu_slOneshot_slOneshot(
    yur_Ref *pt, yur_Ref *h, yur_Ref *B, yur_Ref *A) {
  ((struct yur_Prompt *) pt)->handler = h;
  if (!yur_setjmp(((struct yur_Prompt *) pt)->shift_env))
    yur_longjmp(((struct yur_Prompt *) pt)->reset_env, 1);
  yur_unref(pt);
  return ((struct yur_Prompt *) pt)->argument;
}

#else

static yur_NORETURN void missingSplitStack() {
  yur_panic("Oneshot requires split stack support");
}

void yur_oneshot_jmp(yur_Oneshot_Wrap a, yur_Ref *b, yur_Ref *c) {
  missingSplitStack();
}

int yur_setjmp(yur_Jmp_buf a) {
  missingSplitStack();
}

void yur_longjmp(yur_Jmp_buf a, int b) {
  missingSplitStack();
}

yur_Ref *yu_PromptAxiom_doyu_slOneshot_slOneshot(yur_Ref *a) {
  missingSplitStack();
}

yur_Ref *yu_resetAxiom_doyu_slOneshot_slOneshot(yur_Ref *a, yur_Ref *b) {
  missingSplitStack();
}

yur_Ref *yu_shiftAxiom_doyu_slOneshot_slOneshot(
    yur_Ref *a, yur_Ref *b, yur_Ref *c, yur_Ref *d) {
  missingSplitStack();
}

#endif
