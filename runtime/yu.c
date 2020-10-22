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
  for (size_t i = 0; i < n; i++)
    op(__atomic_load_n(&fs[i], memory_order_relaxed));
}

void yur_unref_children(yur_Ref *r) {
  yur_for_each_child(r, yur_unref);
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

void yur_atomic_memoize(yur_Ref *lazy, yur_Ref **dest, yur_Ref **src,
    yur_Ref *expect) {
  int b = __atomic_compare_exchange_n(dest, &expect, *src, 0,
              memory_order_acq_rel, memory_order_consume);
  if (yur_UNLIKELY(!b)) {
    yur_unref(*src);
    *src = *dest;
  }
  yur_mark_children_atomic(lazy);
}

/////////////////////////// Dynamic Ref //////////////////////////////

yur_ALWAYS_INLINE
static void yur_dynamic_mark_atomic(yur_Ref *r) {
  __atomic_store_n(&r->vmt_index, yur_Atomic_dynamic_vmt, memory_order_relaxed);
  yur_mark_children_atomic(r);
}

///////////////////////// Destructor Ref /////////////////////////////

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

void yur_atomic_dynamic_unref(yur_Ref *r) {
  if (__atomic_sub_fetch(&r->count, 1, memory_order_acq_rel))
    return;
  yur_unref_children(r);
  yur_dealloc(r);
}

////////////////////// Atomic destructor Ref /////////////////////////

void yur_atomic_destructor_unref(yur_Ref *r) {
  if (__atomic_sub_fetch(&r->count, 1, memory_order_acq_rel))
    return;
  ((void (*)(yur_Ref *)) r->tag)(r);
  yur_unref_children(r);
  yur_dealloc(r);
}

//////////////////////////// Methods /////////////////////////////////

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

yur_NORETURN
void yur_panic(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "panic: ");
  vfprintf(stderr, fmt, ap);
  putc('\n', stderr);
  va_end(ap);
  exit(EXIT_FAILURE);
}

yur_Ref *yur_print(yur_Ref *r) {
  putchar((int) r->tag);
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

yur_Ref *yur_malloc(size_t nbytes) {
  yur_Ref *r = (yur_Ref *) mi_malloc(nbytes);
  if (!r)
    yur_panic("memory allocation error");
  r->count = 1;
  r->vmt_index = yur_Dynamic_vmt;
  return r;
}

void yur_dealloc(yur_Ref *r) {
  mi_free(r);
}

///////////////////////////////// main ///////////////////////////////

_Static_assert(sizeof(rlim_t) >= 4, "size of rlim_t is too small");

#define yur_DATA_SIZE ((rlim_t) 1 << 31) // 2GB.

static const rlim_t stack_size = yur_DATA_SIZE;

static const rlim_t data_size = yur_DATA_SIZE;

int main() {
  struct rlimit cs;
  if (getrlimit(RLIMIT_STACK, &cs) == -1)
    yur_panic("failed getting current stack size");

  struct rlimit cd;
  if (getrlimit(RLIMIT_DATA, &cd) == -1)
    yur_panic("failed getting current data size");

  struct rlimit ns = { stack_size, cs.rlim_max };
  if (setrlimit(RLIMIT_STACK, &ns) == -1)
    yur_panic("failed setting stack size, %s", strerror(errno));

  struct rlimit nd = { data_size, cd.rlim_max };
  if (setrlimit(RLIMIT_DATA, &nd) == -1)
    yur_panic("failed setting stack size, %s", strerror(errno));

  yu_main();
}

//////////////////// Basic extern implementations ////////////////////

yur_Ref *yu_undefined_doundefined(yur_Ref *x) {
  yur_unref(x);
  yur_panic("undefined");
}


yur_Ref *yu_depFunext_doEqual(yur_Ref *B2, yur_Ref *B1,
    yur_Ref *A2, yur_Ref *A1, yur_Ref *r, yur_Ref *g, yur_Ref *f) {
  yur_unref(B2);
  yur_unref(B1);
  yur_unref(A2);
  yur_unref(A1);
  yur_unref(r);
  yur_unref(g);
  yur_Ref *x = yur_build(1, 0);
  x->fields[0] = f;
  return x;
}
