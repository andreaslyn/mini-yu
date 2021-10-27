#ifndef YU_YU_H
#define YU_YU_H

#include "yustack.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdatomic.h>

#define yur_NO_SPLIT_STACK __attribute__((no_split_stack))

#define yur_NORETURN __attribute__((noreturn))

#define yur_ALWAYS_INLINE inline __attribute__((always_inline))

#define yur_NOINLINE __attribute__((noinline))

#define yur_NAKED __attribute__((naked))

#define yur_LIKELY(x) __builtin_expect(!!(x), 1)

#define yur_UNLIKELY(x) __builtin_expect(!!(x), 0)

#define yur_ALOAD(x) \
  __atomic_load_n(&(x), memory_order_consume)

#define yur_ASTORE(dest, source) \
  __atomic_store_n(&(dest), (source), memory_order_release)

#define yur_DO_STRINGIFY(x) #x
#define yur_STRINGIFY(x) yur_DO_STRINGIFY(x)

#define yur_SYSTEM_SWITCH(type, name, args) \
yur_NO_SPLIT_STACK yur_NOINLINE yur_NAKED type name args { \
  asm ( \
    "movq %%r15, -8(%%rsp)\n\t" \
    "movq %%rsp, %%r15\n\t" \
    "movq %%fs:0x88, %%rsp\n\t" \
    "call " yur_STRINGIFY(yur_SYSTEM_SWITCH_ ## name) "\n\t" \
    "movq %%r15, %%rsp\n\t" \
    "movq -8(%%rsp), %%r15\n\t" \
    "ret" : : : ); \
} \
yur_NO_SPLIT_STACK type yur_SYSTEM_SWITCH_ ## name args

typedef uint16_t yur_Vmt_index;

typedef uint16_t yur_Num_fields;

enum yur_Vmt_index {
  yur_Dynamic_vmt = 0,
  yur_Static_vmt = 1,
  yur_Destructor_vmt = 2,
  yur_Atomic_dynamic_vmt,
  yur_Atomic_destructor_vmt
};

#define yur_Ref_struct     \
  size_t count;            \
  size_t tag;              \
  yur_Vmt_index vmt_index; \
  yur_Num_fields nfields   \

typedef struct yur_Ref {
  yur_Ref_struct;
  struct yur_Ref *fields[];
} yur_Ref;

yur_Ref *yu_main(yur_Ref *);

// NOTE: call yur_panic with at most 6 arguments!
yur_NORETURN
void yur_panic(const char *fmt, ...);

yur_NORETURN
void yur_SYSTEM_SWITCH_yur_panic(const char *fmt, ...);

yur_Ref *yur_print(yur_Ref *);

extern yur_Ref yur_printref;

extern yur_Ref yur_unit;

yur_Ref *yur_malloc(size_t nbytes);

yur_Ref *yur_alloc(size_t nfields);

void yur_dealloc(yur_Ref *);

static void yur_init(yur_Ref *r, yur_Num_fields nfields, size_t tag);

static yur_Ref *yur_build(yur_Num_fields nfields, size_t tag);

static void yur_inc(yur_Ref *r);

static void yur_unref(yur_Ref *r);

static yur_Ref *yur_reset(yur_Ref *r, yur_Num_fields nfields);

void yur_mark_atomic(yur_Ref *r);

void yur_mark_children_static(yur_Ref *r);

////////////////////////// Ref construction //////////////////////////

yur_ALWAYS_INLINE
static void yur_init(yur_Ref *r, yur_Num_fields nfields, size_t tag) {
  r->tag = tag;
  r->nfields = nfields;
}

yur_ALWAYS_INLINE
static yur_Ref *yur_build(yur_Num_fields nfields, size_t tag) {
  yur_Ref *r = yur_alloc(nfields);
  yur_init(r, nfields, tag);
  return r;
}

////////////////////////////// Util //////////////////////////////////

void yur_atomic_memoize(yur_Ref *lazy, yur_Ref **dest, yur_Ref **src,
    yur_Ref *expect);

inline static void yur_memoize(yur_Ref *lazy, yur_Ref **dest, yur_Ref **src,
    yur_Ref *expect) {
  yur_Vmt_index i = yur_ALOAD(lazy->vmt_index);
  if (yur_LIKELY(i == yur_Dynamic_vmt)) {
    *dest = *src;
  } else if (yur_LIKELY(i == yur_Static_vmt)) {
    __atomic_store_n(dest, *src, memory_order_relaxed);
    yur_mark_children_static(lazy);
  } else if (yur_UNLIKELY(i == yur_Dynamic_vmt)) {
    *dest = *src;
  } else {
    yur_atomic_memoize(lazy, dest, src, expect);
  }
}

void yur_unref_children(yur_Ref *r);

////////////////////////// Dynamic Ref ///////////////////////////////

yur_ALWAYS_INLINE
static void yur_dynamic_inc(yur_Ref *r) {
  ++r->count;
}

yur_ALWAYS_INLINE
static void yur_dynamic_unref(yur_Ref *r) {
  if (--r->count)
    return;
  yur_unref_children(r);
  yur_dealloc(r);
}

///////////////////////// Destructor Ref /////////////////////////////

yur_ALWAYS_INLINE
static void yur_destructor_inc(yur_Ref *r) {
  yur_dynamic_inc(r);
}

void yur_destructor_unref(yur_Ref *r);

/////////////////////// Atomic dynamic Ref ///////////////////////////

yur_ALWAYS_INLINE
static void yur_atomic_dynamic_inc(yur_Ref *r) {
  __atomic_add_fetch(&r->count, 1, memory_order_relaxed);
}

void yur_atomic_dynamic_unref(yur_Ref *r);

////////////////////// Atomic destructor Ref /////////////////////////

yur_ALWAYS_INLINE
static void yur_atomic_destructor_inc(yur_Ref *r) {
  __atomic_add_fetch(&r->count, 1, memory_order_relaxed);
}

void yur_atomic_destructor_unref(yur_Ref *r);

///////////////////////////// Methods ////////////////////////////////

void yur_inc2(yur_Ref *r);

yur_ALWAYS_INLINE
static void yur_inc(yur_Ref *r) {
  yur_Vmt_index i = yur_ALOAD(r->vmt_index);
  if (yur_LIKELY(i == yur_Dynamic_vmt))
    return yur_dynamic_inc(r);
  if (yur_UNLIKELY(i != yur_Static_vmt))
    return yur_inc2(r);
}

void yur_unref2(yur_Ref *r);

yur_ALWAYS_INLINE
static void yur_unref(yur_Ref *r) {
  yur_Vmt_index i = yur_ALOAD(r->vmt_index);
  if (yur_LIKELY(i == yur_Dynamic_vmt))
    return yur_dynamic_unref(r);
  if (yur_UNLIKELY(i != yur_Static_vmt))
    return yur_unref2(r);
}

yur_Ref *yur_reset_alloc(yur_Ref *r, yur_Num_fields nfields);

yur_ALWAYS_INLINE
static yur_Ref *yur_reset(yur_Ref *r, yur_Num_fields nfields) {
  if (yur_ALOAD(r->count) == 1) {
    yur_unref_children(r);
    r->vmt_index = yur_Dynamic_vmt;
    return r;
  }
  return yur_reset_alloc(r, nfields);
}

#endif // YU_YU_H
