#include "yu.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <sys/resource.h>
#include <errno.h>
#include <limits.h>
#include <mimalloc.h>

////////////////////////////// Util //////////////////////////////////

yur_ALWAYS_INLINE
static void yur_for_each_child(yur_Ref *r, void (*op)(yur_Ref *)) {
  size_t n = __atomic_load_n(&r->nfields, memory_order_relaxed);
  yur_Ref **fs = r->fields;
  yur_Ref **end = fs + n;
  while (fs != end) {
    op(__atomic_load_n(fs, memory_order_relaxed));
    ++fs;
  }
}

void yur_unref_children(yur_Ref *r) {
  yur_for_each_child(r, yur_unref);
}

void yur_unref_children_dealloc(yur_Ref *r) {
  yur_for_each_child(r, yur_unref);
  yur_dealloc(r);
}

void yur_destructor_unref_children_dealloc(yur_Ref *r) {
  ((void (*)(yur_Ref *)) r->tag)(r);
  yur_for_each_child(r, yur_unref);
  yur_dealloc(r);
}

static void yur_mark_children_atomic(yur_Ref *r) {
  yur_for_each_child(r, yur_mark_atomic);
}

yur_ALWAYS_INLINE
static void yur_mark_static(yur_Ref *r) {
  if (yur_UNLIKELY(yur_ALOAD(r->vmt_index) == yur_Static_vmt))
    return;
  __atomic_store_n(&r->vmt_index, yur_Static_vmt, memory_order_relaxed);
  __atomic_store_n(&r->count, 0, memory_order_relaxed);
  yur_mark_children_static(r);
}

void yur_mark_children_static(yur_Ref *r) {
  yur_for_each_child(r, yur_mark_static);
}

yur_ALWAYS_INLINE
static void yur_atomic_memoize(yur_Ref *lazy, yur_Ref **dest, yur_Ref **src,
    yur_Ref *expect) {
  int b = __atomic_compare_exchange_n(dest, &expect, *src, 0,
              memory_order_acq_rel, memory_order_consume);
  if (yur_UNLIKELY(!b)) {
    yur_unref(*src);
    *src = *dest;
  }
  yur_mark_children_atomic(lazy);
}

void yur_memoize(yur_Ref *lazy, yur_Ref **dest, yur_Ref **src,
    yur_Ref *expect) {
  yur_Vmt_index i = yur_ALOAD(lazy->vmt_index);
  if (yur_LIKELY(i == yur_Dynamic_vmt)) {
    *dest = *src;
  } else if (yur_LIKELY(i == yur_Static_vmt)) {
    __atomic_store_n(dest, *src, memory_order_relaxed);
    yur_mark_children_static(lazy);
  } else if (yur_UNLIKELY(i == yur_Destructor_vmt)) {
    *dest = *src;
  } else {
    yur_atomic_memoize(lazy, dest, src, expect);
  }
}

/////////////////////////// Dynamic Ref //////////////////////////////

extern yur_ALWAYS_INLINE void yur_dynamic_inc(yur_Ref *r);

extern yur_ALWAYS_INLINE void yur_dynamic_unref(yur_Ref *r);

yur_ALWAYS_INLINE
static void yur_dynamic_mark_atomic(yur_Ref *r) {
  __atomic_store_n(&r->vmt_index, yur_Atomic_dynamic_vmt, memory_order_relaxed);
  yur_mark_children_atomic(r);
}

///////////////////////// Destructor Ref /////////////////////////////

yur_ALWAYS_INLINE
static void yur_destructor_inc(yur_Ref *r) {
  yur_dynamic_inc(r);
}

void yur_destructor_unref(yur_Ref *r) {
  if (--r->count)
    return;
  ((void (*)(yur_Ref *)) r->tag)(r);
  yur_unref_children(r);
  yur_dealloc(r);
}

yur_ALWAYS_INLINE
static void yur_destructor_mark_atomic(yur_Ref *r) {
  __atomic_store_n(&r->vmt_index, yur_Atomic_destructor_vmt, memory_order_relaxed);
  yur_mark_children_atomic(r);
}

/////////////////////// Atomic dynamic Ref ///////////////////////////

yur_ALWAYS_INLINE
static void yur_atomic_dynamic_inc(yur_Ref *r) {
  __atomic_add_fetch(&r->count, 1, memory_order_relaxed);
}

void yur_atomic_dynamic_unref(yur_Ref *r) {
  if (__atomic_sub_fetch(&r->count, 1, memory_order_acq_rel))
    return;
  yur_unref_children(r);
  yur_dealloc(r);
}

////////////////////// Atomic destructor Ref /////////////////////////

yur_ALWAYS_INLINE
static void yur_atomic_destructor_inc(yur_Ref *r) {
  __atomic_add_fetch(&r->count, 1, memory_order_relaxed);
}

void yur_atomic_destructor_unref(yur_Ref *r) {
  if (__atomic_sub_fetch(&r->count, 1, memory_order_acq_rel))
    return;
  ((void (*)(yur_Ref *)) r->tag)(r);
  yur_unref_children(r);
  yur_dealloc(r);
}

//////////////////////////// Methods /////////////////////////////////

extern yur_ALWAYS_INLINE void yur_inc(yur_Ref *r);

extern yur_ALWAYS_INLINE void yur_unref(yur_Ref *r);

void yur_inc_slow(yur_Ref *r, yur_Vmt_index i) {
  if (i == yur_Atomic_destructor_vmt)
    return yur_atomic_destructor_inc(r);
  if (i == yur_Atomic_dynamic_vmt)
    return yur_atomic_dynamic_inc(r);
  return yur_destructor_inc(r);
}

void yur_unref_slow(yur_Ref *r, yur_Vmt_index i) {
  if (i == yur_Atomic_destructor_vmt)
    return yur_atomic_destructor_unref(r);
  if (i == yur_Atomic_dynamic_vmt)
    return yur_atomic_dynamic_unref(r);
  return yur_destructor_unref(r);
}

extern yur_ALWAYS_INLINE
yur_Ref *yur_reset(yur_Ref *r, yur_Num_fields nfields);

yur_Ref *yur_reset_alloc(yur_Ref *r, yur_Num_fields nfields) {
  yur_unref(r);
  struct yur_Ref *s = yur_alloc(nfields);
  s->nfields = nfields;
  return s;
}

yur_ALWAYS_INLINE
void yur_mark_atomic(yur_Ref *r) {
  if (yur_LIKELY(yur_ALOAD(r->vmt_index) == yur_Dynamic_vmt))
    return yur_dynamic_mark_atomic(r);
  if (yur_UNLIKELY(yur_ALOAD(r->vmt_index) == yur_Destructor_vmt))
    return yur_destructor_mark_atomic(r);
}

////////////////////// Basic static runtime data /////////////////////

yur_Ref yur_unit = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im_pa_un0 = {
  .count = 0,
  .tag = (size_t) &yu__pa_un0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im_un = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa_un0
};

yur_SYSTEM_SWITCH_DEF(,
    yur_NORETURN void, yur_panic, (const char *fmt, ...)) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "panic: ");
  vfprintf(stderr, fmt, ap);
  putc('\n', stderr);
  va_end(ap);
  exit(EXIT_FAILURE);
}

yur_SYSTEM_SWITCH_DEF(extern yur_ALWAYS_INLINE, void, yur_putchar, (int c)) {
  putchar(c);
}

yur_SYSTEM_SWITCH_DEF(, yur_Ref *, yur_print, (yur_Ref * r)) {
  yur_putchar((int) r->tag);
  return &yur_unit;
}

static yur_Ref *print_ap(yur_Ref *r, yur_Ref *self) {
  return yur_print(r);
}

yur_Ref yur_printref = {
  .count = 0,
  .tag = (size_t) &print_ap,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

///////////////////////////// Allocator ///////////////////////////

yur_SYSTEM_SWITCH_DEF(extern yur_ALWAYS_INLINE,
    yur_Ref *, yur_malloc, (size_t nbytes)) {
  yur_Ref *r = (yur_Ref *) mi_malloc(nbytes);
  if (!r)
    yur_panic_s("memory allocation error");
  r->count = 1;
  r->vmt_index = yur_Dynamic_vmt;
  return r;
}

yur_SYSTEM_SWITCH_DEF(extern yur_ALWAYS_INLINE,
    yur_Ref *, yur_alloc, (size_t nfields)) {
  return yur_malloc_s(sizeof(yur_Ref) + nfields * sizeof(yur_Ref *));
}

yur_SYSTEM_SWITCH_DEF(extern yur_ALWAYS_INLINE,
    void, yur_dealloc, (yur_Ref *r)) {
  mi_free(r);
}

////////////////////////// Ref construction //////////////////////////

extern yur_ALWAYS_INLINE
void yur_init(yur_Ref *r, yur_Num_fields nfields, size_t tag);

extern yur_ALWAYS_INLINE
yur_Ref *yur_build(yur_Num_fields nfields, size_t tag) {
  yur_Ref *r = yur_alloc(nfields);
  yur_init(r, nfields, tag);
  return r;
}

///////////////////////////////// main ///////////////////////////////

_Static_assert(sizeof(rlim_t) >= 4, "size of rlim_t is too small");

#ifndef yur_DISABLE_SPLIT_STACK

#define yur_DATA_SIZE (6 * ((rlim_t) 1 << 30)) // 6GB.

#else

#define yur_DATA_SIZE (3 * ((rlim_t) 1 << 30)) // 3GB.

#endif

static const rlim_t data_size = yur_DATA_SIZE;

#ifdef yur_DISABLE_SPLIT_STACK
static const rlim_t stack_size = yur_DATA_SIZE;
#endif

#ifndef yur_DISABLE_SPLIT_STACK
int main() {
  struct rlimit cd;
  if (getrlimit(RLIMIT_DATA, &cd) == -1)
    yur_panic_s("failed getting current data size");

  struct rlimit nd = { data_size, cd.rlim_max };
  if (setrlimit(RLIMIT_DATA, &nd) == -1)
    yur_panic_s("failed setting stack size, %s", strerror(errno));

  (void) yur_run_s((yur_Run) yu_main, &yur_unit);
}
#else
int main() {
  struct rlimit cs;
  if (getrlimit(RLIMIT_STACK, &cs) == -1)
    yur_panic_s("failed getting current stack size");

  struct rlimit cd;
  if (getrlimit(RLIMIT_DATA, &cd) == -1)
    yur_panic_s("failed getting current data size");

  struct rlimit ns = { stack_size, cs.rlim_max };
  if (setrlimit(RLIMIT_STACK, &ns) == -1)
    yur_panic_s("failed setting stack size, %s", strerror(errno));

  struct rlimit nd = { data_size, cd.rlim_max };
  if (setrlimit(RLIMIT_DATA, &nd) == -1)
    yur_panic_s("failed setting stack size, %s", strerror(errno));

  yu_main(&yur_unit);
}
#endif

//////////////////// Basic extern implementations ////////////////////

extern yur_ALWAYS_INLINE yur_Ref *yu__un();

extern yur_ALWAYS_INLINE yur_Ref *yu__pa_un0(yur_Ref *self);

yur_Ref *yu_undefinedAxiom_doyu_slundefined_slundefined(yur_Ref *x) {
  yur_unref(x);
  yur_panic("undefined");
}

yur_Ref *yu_funextAxiom_doyu_slbasic_slEqual(yur_Ref *B, yur_Ref *A,
    yur_Ref *r, yur_Ref *g, yur_Ref *f) {
  yur_unref(B);
  yur_unref(A);
  yur_unref(r);
  yur_unref(g);
  yur_Ref *x = yur_build(1, 0);
  x->fields[0] = f;
  return x;
}
