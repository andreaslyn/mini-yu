#ifndef YU_YU_H
#define YU_YU_H

#include "yustack.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdatomic.h>

#define yur_OFFSET_OF(type, field) __builtin_offsetof(type, field)

#define yur_ALOAD(x) \
  __atomic_load_n(&(x), memory_order_consume)

#define yur_ASTORE(dest, source) \
  __atomic_store_n(&(dest), (source), memory_order_release)

typedef uint16_t yur_Vmt_index;

typedef uint16_t yur_Num_fields;

enum yur_Vmt_index {
  yur_Dynamic_vmt = 0,
  yur_Destructor_vmt = 1,
  yur_Static_vmt = 2,
  yur_Atomic_dynamic_vmt = 3,
  yur_Atomic_destructor_vmt = 4
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
yur_SYSTEM_SWITCH_DECL(,
    yur_NORETURN void, yur_panic, (const char *fmt, ...));

yur_SYSTEM_SWITCH_DECL(, void, yur_putchar, (int));

yur_SYSTEM_SWITCH_DECL(, yur_Ref *, yur_print, (yur_Ref *));

extern yur_Ref yur_printref;

extern yur_Ref yur_unit;

yur_ALWAYS_INLINE yur_Ref *yu__un() {
  return &yur_unit;
}

yur_ALWAYS_INLINE yur_Ref *yu__pa_un0(yur_Ref *self) {
  // No need to unref self (which is unit), since it is static.
  return &yur_unit;
}

extern yur_Ref yu__im_pa_un0;

extern yur_Ref yu__im_un;

yur_SYSTEM_SWITCH_DECL(, yur_Ref *, yur_malloc, (size_t nbytes));

yur_SYSTEM_SWITCH_DECL(, yur_Ref *, yur_alloc, (size_t nfields));

yur_SYSTEM_SWITCH_DECL(, void, yur_dealloc, (yur_Ref *));

yur_ALWAYS_INLINE
void yur_init(yur_Ref *r, yur_Num_fields nfields, size_t tag);

yur_Ref *yur_build(yur_Num_fields nfields, size_t tag);

#define yur_inc_1(r) \
  do { \
    register yur_Ref *rdi asm ("rdi") = r; \
    asm volatile ( \
      "call yur_inc_1" \
      : \
      : "r" (rdi) \
      : "cc", "memory" ); \
  } while (0)

#define yur_inc_2(r1, r2) \
  do { \
    register yur_Ref *rdi asm ("rdi") = r1; \
    register yur_Ref *rsi asm ("rsi") = r2; \
    asm volatile ( \
      "call yur_inc_2" \
      : \
      : "r" (rdi), "r" (rsi) \
      : "cc", "memory" ); \
  } while (0)

#define yur_inc_3(r1, r2, r3) \
  do { \
    register yur_Ref *rdi asm ("rdi") = r1; \
    register yur_Ref *rsi asm ("rsi") = r2; \
    register yur_Ref *rdx asm ("rdx") = r3; \
    asm volatile ( \
      "call yur_inc_3" \
      : \
      : "r" (rdi), "r" (rsi), "r" (rdx) \
      : "cc", "memory" ); \
  } while (0)

#define yur_inc_4(r1, r2, r3, r4) \
  do { \
    register yur_Ref *rdi asm ("rdi") = r1; \
    register yur_Ref *rsi asm ("rsi") = r2; \
    register yur_Ref *rdx asm ("rdx") = r3; \
    register yur_Ref *rcx asm ("rcx") = r4; \
    asm volatile ( \
      "call yur_inc_4" \
      : \
      : "r" (rdi), "r" (rsi), "r" (rdx), "r" (rcx) \
      : "cc", "memory" ); \
  } while (0)

#define yur_inc_5(r1, r2, r3, r4, r5) \
  do { \
    register yur_Ref *rdi asm ("rdi") = r1; \
    register yur_Ref *rsi asm ("rsi") = r2; \
    register yur_Ref *rdx asm ("rdx") = r3; \
    register yur_Ref *rcx asm ("rcx") = r4; \
    register yur_Ref *r8 asm ("r8") = r5; \
    asm volatile ( \
      "call yur_inc_5" \
      : \
      : "r" (rdi), "r" (rsi), "r" (rdx), "r" (rcx), \
        "r" (r8) \
      : "cc", "memory" ); \
  } while (0)

#define yur_inc_6(r1, r2, r3, r4, r5, r6) \
  do { \
    register yur_Ref *rdi asm ("rdi") = r1; \
    register yur_Ref *rsi asm ("rsi") = r2; \
    register yur_Ref *rdx asm ("rdx") = r3; \
    register yur_Ref *rcx asm ("rcx") = r4; \
    register yur_Ref *r8 asm ("r8") = r5; \
    register yur_Ref *r9 asm ("r9") = r6; \
    asm volatile ( \
      "call yur_inc_6" \
      : \
      : "r" (rdi), "r" (rsi), "r" (rdx), "r" (rcx), \
        "r" (r8), "r" (r9) \
      : "cc", "memory" ); \
  } while (0)

yur_ALWAYS_INLINE
void yur_inc(yur_Ref *r);

void yur_unref_1(yur_Ref *r);

void yur_unref_2(yur_Ref *r1, yur_Ref *r2);

void yur_unref_3(yur_Ref *r1, yur_Ref *r2, yur_Ref *r3);

void yur_unref_4(yur_Ref *r1, yur_Ref *r2, yur_Ref *r3,
    yur_Ref *r4);

void yur_unref_5(yur_Ref *r1, yur_Ref *r2, yur_Ref *r3,
    yur_Ref *r4, yur_Ref *r5);

void yur_unref_6(yur_Ref *r1, yur_Ref *r2, yur_Ref *r3,
    yur_Ref *r4, yur_Ref *r5, yur_Ref *r6);

yur_ALWAYS_INLINE
void yur_unref(yur_Ref *r);

yur_ALWAYS_INLINE
yur_Ref *yur_reset(yur_Ref *r, yur_Num_fields nfields);

void yur_mark_atomic(yur_Ref *r);

void yur_mark_children_static(yur_Ref *r);

////////////////////////// Ref construction //////////////////////////

yur_ALWAYS_INLINE
void yur_init(yur_Ref *r, yur_Num_fields nfields, size_t tag) {
  r->tag = tag;
  r->nfields = nfields;
}

////////////////////////////// Util //////////////////////////////////

void yur_memoize(yur_Ref *lazy, yur_Ref **dest, yur_Ref **src, yur_Ref *expect);

void yur_unref_children(yur_Ref *r);

void yur_unref_children_dealloc(yur_Ref *r);

void yur_destructor_unref_children_dealloc(yur_Ref *r);

////////////////////////// Dynamic Ref ///////////////////////////////

yur_ALWAYS_INLINE
void yur_dynamic_inc(yur_Ref *r) {
  ++r->count;
}

yur_ALWAYS_INLINE
void yur_dynamic_unref(yur_Ref *r) {
  if (--r->count)
    return;
  yur_unref_children_dealloc(r);
}

///////////////////////////// Methods ////////////////////////////////

void yur_inc_slow(yur_Ref *, yur_Vmt_index);

yur_ALWAYS_INLINE
void yur_inc(yur_Ref *r) {
  yur_Vmt_index i = yur_ALOAD(r->vmt_index);
  if (yur_LIKELY(i == yur_Dynamic_vmt))
    return yur_dynamic_inc(r);
  if (yur_UNLIKELY(i != yur_Static_vmt))
    return yur_inc_slow(r, i);
}

void yur_unref_slow(yur_Ref *, yur_Vmt_index);

yur_ALWAYS_INLINE
void yur_unref(yur_Ref *r) {
   yur_Vmt_index i = yur_ALOAD(r->vmt_index);
   if (yur_LIKELY(i == yur_Dynamic_vmt))
     return yur_dynamic_unref(r);
   if (yur_UNLIKELY(i != yur_Static_vmt))
     return yur_unref_slow(r, i);
}

yur_Ref *yur_reset_alloc(yur_Ref *r, yur_Num_fields nfields);

yur_ALWAYS_INLINE
yur_Ref *yur_reset(yur_Ref *r, yur_Num_fields nfields) {
  if (yur_ALOAD(r->count) == 1) {
    yur_unref_children(r);
    r->vmt_index = yur_Dynamic_vmt;
    return r;
  }
  return yur_reset_alloc(r, nfields);
}

#endif // YU_YU_H
