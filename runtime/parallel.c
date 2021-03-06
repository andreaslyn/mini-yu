#include "yu.h"
#include "yustr.h"
#include <pthread.h>
#include <string.h>
#include <stdio.h>

struct yur_PThread {
  yur_Ref_struct;
  yur_Ref *fn;
  yur_Ref *cache;
  pthread_t thread;
  pthread_mutex_t mutex;
};

static void *thread_fn(void *x0) {
  struct yur_PThread *x = x0;
  yur_Ref *(*f)(yur_Ref *) = (yur_Ref *(*)(yur_Ref *)) x->fn->tag;
  yur_Ref *r = f(x->fn);
  yur_unref((yur_Ref *) x);
  return r;
}

static yur_Ref *mk_error(int e) {
  yur_Ref *r = yur_build(1, 0);
  printf("error %s\n", strerror(e));
  r->fields[0] = yur_cstr_to_yustr(strerror(e));
  return r;
}

static yur_Ref *join_fn(yur_Ref *r) {
  struct yur_PThread *t = (struct yur_PThread *) r->fields[1];
  yur_Ref *result;
  int joinE;

  int mutexE = pthread_mutex_lock(&t->mutex);
  if (mutexE != 0)
    return mk_error(mutexE);

  if (t->cache) {
    joinE = 0;
    result = t->cache;
  } else {
    joinE = pthread_join(t->thread, (void *) &result);
    t->cache = result;
  }
  yur_inc(result);

  mutexE = pthread_mutex_unlock(&t->mutex);
  if (joinE != 0)
    return mk_error(joinE);
  if (mutexE != 0)
    return mk_error(mutexE);

  yur_Ref *s = yur_build(1, 1);
  s->fields[0] = result;
  return s;
}

static yur_Ref join_fn_impl = {
  .count = 0,
  .tag = (size_t) join_fn,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

static yur_Ref *error_fn(yur_Ref *r) {
  yur_Ref *e = r->fields[1];
  yur_inc(e);
  return e;
}

static yur_Ref error_fn_impl = {
  .count = 0,
  .tag = (size_t) error_fn,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

static void destructor_fn(yur_Ref *r) {
  struct yur_PThread *t = (struct yur_PThread *) r;
  if (yur_ALOAD(t->cache)) {
    yur_unref(yur_ALOAD(t->cache));
    return;
  }
  int e = pthread_detach(t->thread);
  if (e != 0)
    fprintf(stderr, "yu error: failed detaching thread: %s\n", strerror(e));
}

static yur_Ref *mk_lazy_error(int e) {
  yur_Ref *r = yur_build(2, 0);
  r->fields[0] = &error_fn_impl;
  r->fields[1] = mk_error(e);
  return r;
}

// (A : Ty, () ->> A) -> () ->> Str | A
yur_Ref *yu_parallel_si_doparallel(yur_Ref *f, yur_Ref *A) {
  yur_unref(A);
  struct yur_PThread *x = (struct yur_PThread *) yur_malloc(sizeof(struct yur_PThread));
  yur_init((yur_Ref *) x, 1, (size_t) destructor_fn);
  x->fn = f;
  x->cache = 0;
  x->vmt_index = yur_Destructor_vmt;
  yur_mark_atomic((yur_Ref *) x);
  yur_inc((yur_Ref *) x);
  int e = pthread_mutex_init(&x->mutex, NULL);
  if (e != 0)
    return mk_lazy_error(e);
  e = pthread_create(&x->thread, NULL, thread_fn, x);
  if (e != 0)
    return mk_lazy_error(e);
  yur_Ref *r = yur_build(2, 0);
  r->fields[0] = &join_fn_impl;
  r->fields[1] = (yur_Ref *) x;
  return r;
}

yur_Ref *yu_parallel_doparallel(yur_Ref *f, yur_Ref *A) {
  return yu_parallel_si_doparallel(f, A);
}
