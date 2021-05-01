#include "yu.h"

yur_Ref *yu_testList();

yur_Ref *yu__patestList0(yur_Ref *self) {
  yur_unref(self);
  return yu_testList();
}

yur_Ref yu__im_patestList0 = {
  .count = 0,
  .tag = (size_t) &yu__patestList0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__imtestList = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_patestList0
};

yur_Ref *yu_timeTestChoice1();

yur_Ref *yu__patimeTestChoice10(yur_Ref *self) {
  yur_unref(self);
  return yu_timeTestChoice1();
}

yur_Ref yu__im_patimeTestChoice10 = {
  .count = 0,
  .tag = (size_t) &yu__patimeTestChoice10,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__imtimeTestChoice1 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_patimeTestChoice10
};

yur_Ref *yu_timeTestChoice2();

yur_Ref *yu__patimeTestChoice20(yur_Ref *self) {
  yur_unref(self);
  return yu_timeTestChoice2();
}

yur_Ref yu__im_patimeTestChoice20 = {
  .count = 0,
  .tag = (size_t) &yu__patimeTestChoice20,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__imtimeTestChoice2 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_patimeTestChoice20
};

yur_Ref *yu_main();

yur_Ref *yu__pamain0(yur_Ref *self) {
  yur_unref(self);
  return yu_main();
}

yur_Ref yu__immain = {
  .count = 0,
  .tag = (size_t) &yu__pamain0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu_1_doBin();

yur_Ref *yu__pa1_doBin0(yur_Ref *self) {
  yur_unref(self);
  return yu_1_doBin();
}

yur_Ref yu__im_pa1_doBin0 = {
  .count = 0,
  .tag = (size_t) &yu__pa1_doBin0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im1_doBin = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa1_doBin0
};

yur_Ref *yu_10_doBin();

yur_Ref *yu__pa10_doBin0(yur_Ref *self) {
  yur_unref(self);
  return yu_10_doBin();
}

yur_Ref yu__im_pa10_doBin0 = {
  .count = 0,
  .tag = (size_t) &yu__pa10_doBin0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im10_doBin = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa10_doBin0
};

yur_Ref *yu__pr_mi_mi_mi_baBin_pl(yur_Ref *x1645);

yur_Ref *yu__pa_pr_mi_mi_mi_baBin_pl0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__pr_mi_mi_mi_baBin_pl(a1);
}

yur_Ref yu__im_pr_mi_mi_mi_baBin_pl = {
  .count = 0,
  .tag = (size_t) &yu__pa_pr_mi_mi_mi_baBin_pl0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__pr_mi_mi_baBin(yur_Ref *x1650);

yur_Ref *yu__pa_pr_mi_mi_baBin0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__pr_mi_mi_baBin(a1);
}

yur_Ref yu__im_pr_mi_mi_baBin = {
  .count = 0,
  .tag = (size_t) &yu__pa_pr_mi_mi_baBin0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__in_as_baBin(yur_Ref *x1664, yur_Ref *x1663);

yur_Ref *yu__pa_in_as_baBin0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__in_as_baBin(a2, a1);
}

yur_Ref yu__im_in_as_baBin = {
  .count = 0,
  .tag = (size_t) &yu__pa_in_as_baBin0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__pocmp_baBin(yur_Ref *x1747, yur_Ref *x1746);

yur_Ref *yu__pa_pocmp_baBin0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__pocmp_baBin(a2, a1);
}

yur_Ref yu__im_pocmp_baBin = {
  .count = 0,
  .tag = (size_t) &yu__pa_pocmp_baBin0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu_2_doBin_pl();

yur_Ref *yu__pa2_doBin_pl0(yur_Ref *self) {
  yur_unref(self);
  return yu_2_doBin_pl();
}

yur_Ref yu__im_pa2_doBin_pl0 = {
  .count = 0,
  .tag = (size_t) &yu__pa2_doBin_pl0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im2_doBin_pl = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa2_doBin_pl0
};

yur_Ref *yu_10_doBin_pl();

yur_Ref *yu__pa10_doBin_pl0(yur_Ref *self) {
  yur_unref(self);
  return yu_10_doBin_pl();
}

yur_Ref yu__im_pa10_doBin_pl0 = {
  .count = 0,
  .tag = (size_t) &yu__pa10_doBin_pl0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im10_doBin_pl = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa10_doBin_pl0
};

yur_Ref *yu__pr_pl_pl_baBin_pl(yur_Ref *x2202);

yur_Ref *yu__pa_pr_pl_pl_baBin_pl0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__pr_pl_pl_baBin_pl(a1);
}

yur_Ref yu__im_pr_pl_pl_baBin_pl = {
  .count = 0,
  .tag = (size_t) &yu__pa_pr_pl_pl_baBin_pl0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__in_pl_baBin_pl(yur_Ref *x2214, yur_Ref *x2213);

yur_Ref *yu__pa_in_pl_baBin_pl0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__in_pl_baBin_pl(a2, a1);
}

yur_Ref yu__im_in_pl_baBin_pl = {
  .count = 0,
  .tag = (size_t) &yu__pa_in_pl_baBin_pl0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__in_as_baBin_pl(yur_Ref *x2244, yur_Ref *x2243);

yur_Ref *yu__pa_in_as_baBin_pl0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__in_as_baBin_pl(a2, a1);
}

yur_Ref yu__im_in_as_baBin_pl = {
  .count = 0,
  .tag = (size_t) &yu__pa_in_as_baBin_pl0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__pr_mi_mi_baBin_pl(yur_Ref *x2266);

yur_Ref *yu__pa_pr_mi_mi_baBin_pl0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__pr_mi_mi_baBin_pl(a1);
}

yur_Ref yu__im_pr_mi_mi_baBin_pl = {
  .count = 0,
  .tag = (size_t) &yu__pa_pr_mi_mi_baBin_pl0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__pocmp_baBin_pl(yur_Ref *x2278, yur_Ref *x2277);

yur_Ref *yu__pa_pocmp_baBin_pl0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__pocmp_baBin_pl(a2, a1);
}

yur_Ref yu__im_pocmp_baBin_pl = {
  .count = 0,
  .tag = (size_t) &yu__pa_pocmp_baBin_pl0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu_Nondet(yur_Ref *x2473, yur_Ref *x2472);

yur_Ref *yu__paNondet0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu_Nondet(a2, a1);
}

yur_Ref yu__imNondet = {
  .count = 0,
  .tag = (size_t) &yu__paNondet0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu_isFunctorEnv_doNondetC(yur_Ref *x2664, yur_Ref *x2663, yur_Ref *x2662, yur_Ref *x2661);

yur_Ref *yu__paisFunctorEnv_doNondetC0(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu_isFunctorEnv_doNondetC(a4, a3, a2, a1);
}

yur_Ref yu__imisFunctorEnv_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__paisFunctorEnv_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__in_co_gr_baTy(yur_Ref *x4000, yur_Ref *x3999);

yur_Ref *yu__pa_in_co_gr_baTy0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__in_co_gr_baTy(a2, a1);
}

yur_Ref yu__im_in_co_gr_baTy = {
  .count = 0,
  .tag = (size_t) &yu__pa_in_co_gr_baTy0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu_isFunctor_doid(yur_Ref *x4439, yur_Ref *x4438, yur_Ref *x4437, yur_Ref *x4436);

yur_Ref *yu__paisFunctor_doid0(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu_isFunctor_doid(a4, a3, a2, a1);
}

yur_Ref yu__imisFunctor_doid = {
  .count = 0,
  .tag = (size_t) &yu__paisFunctor_doid0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__poprint_baChar(yur_Ref *x14617);

yur_Ref *yu__pa_poprint_baChar0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__poprint_baChar(a1);
}

yur_Ref yu__im_poprint_baChar = {
  .count = 0,
  .tag = (size_t) &yu__pa_poprint_baChar0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu_aux0____poprint_baStr(yur_Ref *x4682);

yur_Ref *yu__paaux0____poprint_baStr0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu_aux0____poprint_baStr(a1);
}

yur_Ref yu__imaux0____poprint_baStr = {
  .count = 0,
  .tag = (size_t) &yu__paaux0____poprint_baStr0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__postr_baNat(yur_Ref *x6279);

yur_Ref *yu__pa_postr_baNat0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__postr_baNat(a1);
}

yur_Ref yu__im_postr_baNat = {
  .count = 0,
  .tag = (size_t) &yu__pa_postr_baNat0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu_aux0____podivmod_baNat(yur_Ref *x7570, yur_Ref *x7569, yur_Ref *x7568, yur_Ref *x7566);

yur_Ref *yu__paaux0____podivmod_baNat0(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu_aux0____podivmod_baNat(a4, a3, a2, a1);
}

yur_Ref yu__imaux0____podivmod_baNat = {
  .count = 0,
  .tag = (size_t) &yu__paaux0____podivmod_baNat0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__poeq_qu_baOrder(yur_Ref *x13611);

yur_Ref *yu__pa_poeq_qu_baOrder0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__poeq_qu_baOrder(a1);
}

yur_Ref yu__im_poeq_qu_baOrder = {
  .count = 0,
  .tag = (size_t) &yu__pa_poeq_qu_baOrder0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu_1();

yur_Ref *yu__pa10(yur_Ref *self) {
  yur_unref(self);
  return yu_1();
}

yur_Ref yu__im_pa10 = {
  .count = 0,
  .tag = (size_t) &yu__pa10,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im1 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa10
};

yur_Ref *yu_2();

yur_Ref *yu__pa20(yur_Ref *self) {
  yur_unref(self);
  return yu_2();
}

yur_Ref yu__im_pa20 = {
  .count = 0,
  .tag = (size_t) &yu__pa20,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im2 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa20
};

yur_Ref *yu_3();

yur_Ref *yu__pa30(yur_Ref *self) {
  yur_unref(self);
  return yu_3();
}

yur_Ref yu__im_pa30 = {
  .count = 0,
  .tag = (size_t) &yu__pa30,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im3 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa30
};

yur_Ref *yu_4();

yur_Ref *yu__pa40(yur_Ref *self) {
  yur_unref(self);
  return yu_4();
}

yur_Ref yu__im_pa40 = {
  .count = 0,
  .tag = (size_t) &yu__pa40,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im4 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa40
};

yur_Ref *yu_5();

yur_Ref *yu__pa50(yur_Ref *self) {
  yur_unref(self);
  return yu_5();
}

yur_Ref yu__im_pa50 = {
  .count = 0,
  .tag = (size_t) &yu__pa50,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im5 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa50
};

yur_Ref *yu_6();

yur_Ref *yu__pa60(yur_Ref *self) {
  yur_unref(self);
  return yu_6();
}

yur_Ref yu__im_pa60 = {
  .count = 0,
  .tag = (size_t) &yu__pa60,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im6 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa60
};

yur_Ref *yu_7();

yur_Ref *yu__pa70(yur_Ref *self) {
  yur_unref(self);
  return yu_7();
}

yur_Ref yu__im_pa70 = {
  .count = 0,
  .tag = (size_t) &yu__pa70,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im7 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa70
};

yur_Ref *yu_8();

yur_Ref *yu__pa80(yur_Ref *self) {
  yur_unref(self);
  return yu_8();
}

yur_Ref yu__im_pa80 = {
  .count = 0,
  .tag = (size_t) &yu__pa80,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im8 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa80
};

yur_Ref *yu_9();

yur_Ref *yu__pa90(yur_Ref *self) {
  yur_unref(self);
  return yu_9();
}

yur_Ref yu__im_pa90 = {
  .count = 0,
  .tag = (size_t) &yu__pa90,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im9 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa90
};

yur_Ref *yu_10();

yur_Ref *yu__pa100(yur_Ref *self) {
  yur_unref(self);
  return yu_10();
}

yur_Ref yu__im_pa100 = {
  .count = 0,
  .tag = (size_t) &yu__pa100,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__im10 = {
  .count = 0,
  .tag = 0,
  .vmt_index = yur_Static_vmt,
  .nfields = 1,
  .fields[0] = (yur_Ref *) &yu__im_pa100
};

yur_Ref *yu__in_mi_baNat(yur_Ref *x13901, yur_Ref *x13900);

yur_Ref *yu__pa_in_mi_baNat0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__in_mi_baNat(a2, a1);
}

yur_Ref yu__im_in_mi_baNat = {
  .count = 0,
  .tag = (size_t) &yu__pa_in_mi_baNat0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0makeTestList(yur_Ref *x1);

yur_Ref *yu__fn1makeTestList(yur_Ref *x1, yur_Ref *x0);

yur_Ref *yu__fn13timeTestChoice1(yur_Ref *x293, yur_Ref *x152, yur_Ref *x151, yur_Ref *x105, yur_Ref *x67);

yur_Ref *yu__pa_fn13timeTestChoice10(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn13timeTestChoice1(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn13timeTestChoice1 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn13timeTestChoice10,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn17timeTestChoice1(yur_Ref *x434, yur_Ref *x293, yur_Ref *x152, yur_Ref *x151, yur_Ref *x105, yur_Ref *x67);

yur_Ref *yu__pa_fn17timeTestChoice10(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn17timeTestChoice1(a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn17timeTestChoice1 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn17timeTestChoice10,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn21timeTestChoice1(yur_Ref *x575, yur_Ref *x434, yur_Ref *x293, yur_Ref *x152, yur_Ref *x151, yur_Ref *x105, yur_Ref *x67);

yur_Ref *yu__pa_fn21timeTestChoice10(yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn21timeTestChoice1(a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn21timeTestChoice1 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn21timeTestChoice10,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn25timeTestChoice1(yur_Ref *x730, yur_Ref *x575, yur_Ref *x434, yur_Ref *x293, yur_Ref *x152, yur_Ref *x151, yur_Ref *x105, yur_Ref *x67);

yur_Ref *yu__pa_fn25timeTestChoice10(yur_Ref *a8, yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn25timeTestChoice1(a8, a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn25timeTestChoice1 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn25timeTestChoice10,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn13timeTestChoice2(yur_Ref *x1057, yur_Ref *x916, yur_Ref *x915, yur_Ref *x869, yur_Ref *x831);

yur_Ref *yu__pa_fn13timeTestChoice20(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn13timeTestChoice2(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn13timeTestChoice2 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn13timeTestChoice20,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn17timeTestChoice2(yur_Ref *x1198, yur_Ref *x916, yur_Ref *x915, yur_Ref *x869, yur_Ref *x831);

yur_Ref *yu__pa_fn17timeTestChoice20(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn17timeTestChoice2(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn17timeTestChoice2 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn17timeTestChoice20,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn21timeTestChoice2(yur_Ref *x1339, yur_Ref *x916, yur_Ref *x915, yur_Ref *x869, yur_Ref *x831);

yur_Ref *yu__pa_fn21timeTestChoice20(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn21timeTestChoice2(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn21timeTestChoice2 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn21timeTestChoice20,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn25timeTestChoice2(yur_Ref *x1494, yur_Ref *x916, yur_Ref *x915, yur_Ref *x869, yur_Ref *x831);

yur_Ref *yu__pa_fn25timeTestChoice20(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn25timeTestChoice2(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn25timeTestChoice2 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn25timeTestChoice20,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn2_poalt_baIn(yur_Ref *x2529, yur_Ref *x2510, yur_Ref *x2509);

yur_Ref *yu__pa_fn2_poalt_baIn0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn2_poalt_baIn(a3, a2, a1);
}

yur_Ref yu__im_fn2_poalt_baIn = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn2_poalt_baIn0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0chooseAll_doNondetC(yur_Ref *x2582, yur_Ref *x2581, yur_Ref *x2580, yur_Ref *x2579, yur_Ref *x2578, yur_Ref *x2577, yur_Ref *x2576);

yur_Ref *yu__pa_fn0chooseAll_doNondetC0(yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0chooseAll_doNondetC(a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0chooseAll_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0chooseAll_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn2chooseAll_doNondetC(yur_Ref *x2618, yur_Ref *x2597, yur_Ref *x2581, yur_Ref *x2580, yur_Ref *x2579, yur_Ref *x2578, yur_Ref *x2577, yur_Ref *x2576);

yur_Ref *yu__pa_fn2chooseAll_doNondetC0(yur_Ref *a8, yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn2chooseAll_doNondetC(a8, a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn2chooseAll_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn2chooseAll_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0unit_doNondetC(yur_Ref *x2675, yur_Ref *x2674, yur_Ref *x2673, yur_Ref *x2672, yur_Ref *x2671);

yur_Ref *yu__pa_fn0unit_doNondetC0(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0unit_doNondetC(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0unit_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0unit_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0modify_doNondetC(yur_Ref *x2698, yur_Ref *x2697, yur_Ref *x2696, yur_Ref *x2695, yur_Ref *x2694, yur_Ref *x2693, yur_Ref *x2692);

yur_Ref *yu__pa_fn0modify_doNondetC0(yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0modify_doNondetC(a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0modify_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0modify_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn1modify_doNondetC(yur_Ref *x2727, yur_Ref *x2726, yur_Ref *x2725, yur_Ref *x2724);

yur_Ref *yu__pa_fn1modify_doNondetC0(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn1modify_doNondetC(a4, a3, a2, a1);
}

yur_Ref yu__im_fn1modify_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1modify_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0shift_doNondetC(yur_Ref *x2756, yur_Ref *x2755, yur_Ref *x2754, yur_Ref *x2753, yur_Ref *x2752, yur_Ref *x2751, yur_Ref *x2750);

yur_Ref *yu__pa_fn0shift_doNondetC0(yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0shift_doNondetC(a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0shift_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0shift_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0reset_doNondetC(yur_Ref *x2836, yur_Ref *x2835, yur_Ref *x2834, yur_Ref *x2833, yur_Ref *x2832, yur_Ref *x2831, yur_Ref *x2830, yur_Ref *x2829, yur_Ref *x2828, yur_Ref *x2827, yur_Ref *x2826);

yur_Ref *yu__pa_fn0reset_doNondetC0(yur_Ref *a11, yur_Ref *a10, yur_Ref *a9, yur_Ref *a8, yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0reset_doNondetC(a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0reset_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0reset_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0State(yur_Ref *x2874, yur_Ref *x2873);

yur_Ref *yu__pa_fn0State0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0State(a2, a1);
}

yur_Ref yu__im_fn0State = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0State0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0isFunctorEnv_doStateC(yur_Ref *x2929, yur_Ref *x2928, yur_Ref *x2927, yur_Ref *x2926, yur_Ref *x2925);

yur_Ref *yu__pa_fn0isFunctorEnv_doStateC0(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0isFunctorEnv_doStateC(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0isFunctorEnv_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0isFunctorEnv_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0unit_doStateC(yur_Ref *x2956, yur_Ref *x2955, yur_Ref *x2954, yur_Ref *x2953, yur_Ref *x2952, yur_Ref *x2951);

yur_Ref *yu__pa_fn0unit_doStateC0(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0unit_doStateC(a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0unit_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0unit_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0modify_doStateC(yur_Ref *x2996, yur_Ref *x2995, yur_Ref *x2994, yur_Ref *x2993, yur_Ref *x2992, yur_Ref *x2991, yur_Ref *x2990, yur_Ref *x2989);

yur_Ref *yu__pa_fn0modify_doStateC0(yur_Ref *a8, yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0modify_doStateC(a8, a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0modify_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0modify_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0runC0___modify_doStateC(yur_Ref *x3001, yur_Ref *x3000, yur_Ref *x2999, yur_Ref *x2998, yur_Ref *x2997, yur_Ref *x2990, yur_Ref *x2989);

yur_Ref *yu__pa_fn0runC0___modify_doStateC0(yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0runC0___modify_doStateC(a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0runC0___modify_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0runC0___modify_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn2runC0___modify_doStateC(yur_Ref *x3014, yur_Ref *x2997);

yur_Ref *yu__pa_fn2runC0___modify_doStateC0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn2runC0___modify_doStateC(a2, a1);
}

yur_Ref yu__im_fn2runC0___modify_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn2runC0___modify_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0shift_doStateC(yur_Ref *x3134, yur_Ref *x3133, yur_Ref *x3132, yur_Ref *x3131, yur_Ref *x3130, yur_Ref *x3129, yur_Ref *x3128, yur_Ref *x3127);

yur_Ref *yu__pa_fn0shift_doStateC0(yur_Ref *a8, yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0shift_doStateC(a8, a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0shift_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0shift_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn4shift_doStateC(yur_Ref *x3169, yur_Ref *x3151);

yur_Ref *yu__pa_fn4shift_doStateC0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn4shift_doStateC(a2, a1);
}

yur_Ref yu__im_fn4shift_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn4shift_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn6shift_doStateC(yur_Ref *x3193, yur_Ref *x3177, yur_Ref *x3134, yur_Ref *x3132, yur_Ref *x3131, yur_Ref *x3130, yur_Ref *x3128, yur_Ref *x3127);

yur_Ref *yu__pa_fn6shift_doStateC0(yur_Ref *a8, yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn6shift_doStateC(a8, a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn6shift_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn6shift_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn8shift_doStateC(yur_Ref *x3211, yur_Ref *x3177);

yur_Ref *yu__pa_fn8shift_doStateC0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn8shift_doStateC(a2, a1);
}

yur_Ref yu__im_fn8shift_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn8shift_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0reset_doStateC(yur_Ref *x3230, yur_Ref *x3229, yur_Ref *x3228, yur_Ref *x3227, yur_Ref *x3226, yur_Ref *x3225, yur_Ref *x3224, yur_Ref *x3223, yur_Ref *x3222, yur_Ref *x3221, yur_Ref *x3220);

yur_Ref *yu__pa_fn0reset_doStateC0(yur_Ref *a11, yur_Ref *a10, yur_Ref *a9, yur_Ref *a8, yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0reset_doStateC(a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0reset_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0reset_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn1run_doStateC(yur_Ref *x3276, yur_Ref *x3260);

yur_Ref *yu__pa_fn1run_doStateC0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn1run_doStateC(a2, a1);
}

yur_Ref yu__im_fn1run_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1run_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0performShift(yur_Ref *x3452, yur_Ref *x3451, yur_Ref *x3450, yur_Ref *x3449, yur_Ref *x3446);

yur_Ref *yu__pa_fn0performShift0(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0performShift(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0performShift = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0performShift0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0isFunctor_doComp(yur_Ref *x4212);

yur_Ref *yu__pa_fn0isFunctor_doComp0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0isFunctor_doComp(a1);
}

yur_Ref yu__im_fn0isFunctor_doComp = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0isFunctor_doComp0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn1map_doContT(yur_Ref *x4523, yur_Ref *x4522, yur_Ref *x4521, yur_Ref *x4520, yur_Ref *x4519, yur_Ref *x4518, yur_Ref *x4517);

yur_Ref *yu__pa_fn1map_doContT0(yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn1map_doContT(a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn1map_doContT = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1map_doContT0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0isFunctor_doContT(yur_Ref *x4541, yur_Ref *x4540, yur_Ref *x4539, yur_Ref *x4538, yur_Ref *x4537, yur_Ref *x4536);

yur_Ref *yu__pa_fn0isFunctor_doContT0(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0isFunctor_doContT(a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_fn0isFunctor_doContT = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0isFunctor_doContT0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn1isFunctor_doContT(yur_Ref *x4545, yur_Ref *x4537);

yur_Ref *yu__pa_fn1isFunctor_doContT0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn1isFunctor_doContT(a2, a1);
}

yur_Ref yu__im_fn1isFunctor_doContT = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1isFunctor_doContT0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn1pure_doContT(yur_Ref *x4557, yur_Ref *x4556);

yur_Ref *yu__pa_fn1pure_doContT0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn1pure_doContT(a2, a1);
}

yur_Ref yu__im_fn1pure_doContT = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1pure_doContT0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn1bind_doContT(yur_Ref *x4566, yur_Ref *x4565, yur_Ref *x4564);

yur_Ref *yu__pa_fn1bind_doContT0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn1bind_doContT(a3, a2, a1);
}

yur_Ref yu__im_fn1bind_doContT = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1bind_doContT0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn2bind_doContT(yur_Ref *x4569, yur_Ref *x4566, yur_Ref *x4565);

yur_Ref *yu__pa_fn2bind_doContT0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn2bind_doContT(a3, a2, a1);
}

yur_Ref yu__im_fn2bind_doContT = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn2bind_doContT0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0_po2_ba_pr_am_ba_in_mi_gr(yur_Ref *x13685);

yur_Ref *yu__pa_fn0_po2_ba_pr_am_ba_in_mi_gr0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0_po2_ba_pr_am_ba_in_mi_gr(a1);
}

yur_Ref yu__im_fn0_po2_ba_pr_am_ba_in_mi_gr = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0_po2_ba_pr_am_ba_in_mi_gr0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn0_in_co_co_ba_poList_baTy(yur_Ref *x13938, yur_Ref *x13937);

yur_Ref *yu__pa_fn0_in_co_co_ba_poList_baTy0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn0_in_co_co_ba_poList_baTy(a2, a1);
}

yur_Ref yu__im_fn0_in_co_co_ba_poList_baTy = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0_in_co_co_ba_poList_baTy0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__fn1_poelse_baThen_doBool(yur_Ref *x14011, yur_Ref *x14004, yur_Ref *x14001);

yur_Ref *yu__fn1_in_at_ba_in_mi_gr(yur_Ref *x14220, yur_Ref *x14219, yur_Ref *x14218);

yur_Ref *yu__pa_fn1_in_at_ba_in_mi_gr0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__fn1_in_at_ba_in_mi_gr(a3, a2, a1);
}

yur_Ref yu__im_fn1_in_at_ba_in_mi_gr = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1_in_at_ba_in_mi_gr0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0reset_doNondetC(yur_Ref *x20194);

yur_Ref *yu__pa_du_fn0reset_doNondetC0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0reset_doNondetC(a1);
}

yur_Ref yu__im_du_fn0reset_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0reset_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn1unit_doStateC(yur_Ref *x20225, yur_Ref *x20224, yur_Ref *x20223, yur_Ref *x20222);

yur_Ref *yu__pa_du_fn1unit_doStateC0(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn1unit_doStateC(a4, a3, a2, a1);
}

yur_Ref yu__im_du_fn1unit_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn1unit_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn9modify_doStateC(yur_Ref *x20301, yur_Ref *x20300);

yur_Ref *yu__pa_du_fn9modify_doStateC0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn9modify_doStateC(a2, a1);
}

yur_Ref yu__im_du_fn9modify_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn9modify_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0shift_doStateC(yur_Ref *x20313, yur_Ref *x20312, yur_Ref *x20311, yur_Ref *x20310, yur_Ref *x20309, yur_Ref *x20308, yur_Ref *x20307);

yur_Ref *yu__pa_du_fn0shift_doStateC0(yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0shift_doStateC(a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_du_fn0shift_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0shift_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0reset_doStateC(yur_Ref *x20373);

yur_Ref *yu__pa_du_fn0reset_doStateC0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0reset_doStateC(a1);
}

yur_Ref yu__im_du_fn0reset_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0reset_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn1handle_doHandler(yur_Ref *x20409, yur_Ref *x20408, yur_Ref *x20407, yur_Ref *x20406, yur_Ref *x20405);

yur_Ref *yu__pa_du_fn1handle_doHandler0(yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn1handle_doHandler(a5, a4, a3, a2, a1);
}

yur_Ref yu__im_du_fn1handle_doHandler = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn1handle_doHandler0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn5performShift(yur_Ref *x20420, yur_Ref *x20419, yur_Ref *x20418, yur_Ref *x20417, yur_Ref *x20416, yur_Ref *x20415);

yur_Ref *yu__pa_du_fn5performShift0(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn5performShift(a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_du_fn5performShift = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn5performShift0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn9performShift(yur_Ref *x20435, yur_Ref *x20434, yur_Ref *x20433, yur_Ref *x20432, yur_Ref *x20431, yur_Ref *x20430, yur_Ref *x20429);

yur_Ref *yu__pa_du_fn9performShift0(yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn9performShift(a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_du_fn9performShift = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn9performShift0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn14performShift(yur_Ref *x20450, yur_Ref *x20449, yur_Ref *x20448, yur_Ref *x20447, yur_Ref *x20446, yur_Ref *x20445, yur_Ref *x20444, yur_Ref *x20443);

yur_Ref *yu__pa_du_fn14performShift0(yur_Ref *a8, yur_Ref *a7, yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn14performShift(a8, a7, a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_du_fn14performShift = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn14performShift0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0pure_doComp(yur_Ref *x20467, yur_Ref *x20466);

yur_Ref *yu__pa_du_fn0pure_doComp0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0pure_doComp(a2, a1);
}

yur_Ref yu__im_du_fn0pure_doComp = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0pure_doComp0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0bind_doComp(yur_Ref *x20477, yur_Ref *x20476, yur_Ref *x20475);

yur_Ref *yu__pa_du_fn0bind_doComp0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0bind_doComp(a3, a2, a1);
}

yur_Ref yu__im_du_fn0bind_doComp = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0bind_doComp0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0_polen_ba_poList_baTy(yur_Ref *x20505);

yur_Ref *yu__pa_du_fn0_polen_ba_poList_baTy0(yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0_polen_ba_poList_baTy(a1);
}

yur_Ref yu__im_du_fn0_polen_ba_poList_baTy = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0_polen_ba_poList_baTy0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0_pomap_ba_poList_baTy(yur_Ref *x20513, yur_Ref *x20512);

yur_Ref *yu__pa_du_fn0_pomap_ba_poList_baTy0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0_pomap_ba_poList_baTy(a2, a1);
}

yur_Ref yu__im_du_fn0_pomap_ba_poList_baTy = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0_pomap_ba_poList_baTy0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0_pofold_ba_poList_baTy(yur_Ref *x20524, yur_Ref *x20523, yur_Ref *x20522);

yur_Ref *yu__pa_du_fn0_pofold_ba_poList_baTy0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0_pofold_ba_poList_baTy(a3, a2, a1);
}

yur_Ref yu__im_du_fn0_pofold_ba_poList_baTy = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0_pofold_ba_poList_baTy0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0_poelse_baThen_doBool(yur_Ref *x20538, yur_Ref *x20537);

yur_Ref *yu__pa_du_fn0_poelse_baThen_doBool0(yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0_poelse_baThen_doBool(a2, a1);
}

yur_Ref yu__im_du_fn0_poelse_baThen_doBool = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0_poelse_baThen_doBool0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_du_fn6modify_doStateC(yur_Ref *x21913, yur_Ref *x21912, yur_Ref *x21911);

yur_Ref *yu__pa_du_du_fn6modify_doStateC0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_du_fn6modify_doStateC(a3, a2, a1);
}

yur_Ref yu__im_du_du_fn6modify_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_du_fn6modify_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_du_fn2shift_doStateC(yur_Ref *x21921, yur_Ref *x21920, yur_Ref *x21919);

yur_Ref *yu__pa_du_du_fn2shift_doStateC0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_du_fn2shift_doStateC(a3, a2, a1);
}

yur_Ref yu__im_du_du_fn2shift_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_du_fn2shift_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn0_pochoose_baIn(yur_Ref *x22350, yur_Ref *x22349, yur_Ref *x22348, yur_Ref *x22347);

yur_Ref *yu__pa_du_fn0_pochoose_baIn0(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn0_pochoose_baIn(a4, a3, a2, a1);
}

yur_Ref yu__im_du_fn0_pochoose_baIn = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn0_pochoose_baIn0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_fn2modify_doStateC(yur_Ref *x22369, yur_Ref *x22368, yur_Ref *x22367, yur_Ref *x22366, yur_Ref *x22365, yur_Ref *x22364);

yur_Ref *yu__pa_du_fn2modify_doStateC0(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_fn2modify_doStateC(a6, a5, a4, a3, a2, a1);
}

yur_Ref yu__im_du_fn2modify_doStateC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_fn2modify_doStateC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_du_fn4chooseAll_doNondetC(yur_Ref *x22380, yur_Ref *x22379, yur_Ref *x22378);

yur_Ref *yu__pa_du_du_fn4chooseAll_doNondetC0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_du_fn4chooseAll_doNondetC(a3, a2, a1);
}

yur_Ref yu__im_du_du_fn4chooseAll_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_du_fn4chooseAll_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_du_fn4shift_doNondetC(yur_Ref *x22398, yur_Ref *x22397, yur_Ref *x22396);

yur_Ref *yu__pa_du_du_fn4shift_doNondetC0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_du_fn4shift_doNondetC(a3, a2, a1);
}

yur_Ref yu__im_du_du_fn4shift_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_du_fn4shift_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_du_fn2shift_doNondetC(yur_Ref *x22577, yur_Ref *x22576, yur_Ref *x22575);

yur_Ref *yu__pa_du_du_fn2shift_doNondetC0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_du_fn2shift_doNondetC(a3, a2, a1);
}

yur_Ref yu__im_du_du_fn2shift_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_du_fn2shift_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__du_du_du_fn0shift_doNondetC(yur_Ref *x22767, yur_Ref *x22766, yur_Ref *x22765);

yur_Ref *yu__pa_du_du_du_fn0shift_doNondetC0(yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_unref(self);
  return yu__du_du_du_fn0shift_doNondetC(a3, a2, a1);
}

yur_Ref yu__im_du_du_du_fn0shift_doNondetC = {
  .count = 0,
  .tag = (size_t) &yu__pa_du_du_du_fn0shift_doNondetC0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct1 = {
  .count = 0,
  .tag = 1,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct63 = {
  .count = 0,
  .tag = 63,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct2 = {
  .count = 0,
  .tag = 2,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct3 = {
  .count = 0,
  .tag = 3,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct4 = {
  .count = 0,
  .tag = 4,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct5 = {
  .count = 0,
  .tag = 5,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct6 = {
  .count = 0,
  .tag = 6,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct7 = {
  .count = 0,
  .tag = 7,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct8 = {
  .count = 0,
  .tag = 8,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref yu__ct9 = {
  .count = 0,
  .tag = 9,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__pa_fn0makeTestList1(yur_Ref *self) {
  yur_Ref *b0 = self->fields[1];
  yur_inc(b0);
  return yu__fn0makeTestList(b0);
}

yur_Ref yu__im_pa_fn0makeTestList1 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn0makeTestList1,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__pa_fn1makeTestList2(yur_Ref *self) {
  yur_Ref *b0 = self->fields[1];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[2];
  yur_inc(b1);
  return yu__fn1makeTestList(b1, b0);
}

yur_Ref yu__im_pa_fn1makeTestList2 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1makeTestList2,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__pa_fn0isFunctorEnv_doStateC1(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn0isFunctorEnv_doStateC(a4, a3, a2, a1, b0);
}

yur_Ref *yu__pa_fn0unit_doStateC2(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__fn0unit_doStateC(a4, a3, a2, a1, b1, b0);
}

yur_Ref *yu__pa_fn0modify_doStateC2(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__fn0modify_doStateC(a6, a5, a4, a3, a2, a1, b1, b0);
}

yur_Ref *yu__pa_fn0shift_doStateC2(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__fn0shift_doStateC(a6, a5, a4, a3, a2, a1, b1, b0);
}

yur_Ref *yu__pa_fn0unit_doNondetC1(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn0unit_doNondetC(a4, a3, a2, a1, b0);
}

yur_Ref *yu__pa_fn0modify_doNondetC1(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn0modify_doNondetC(a6, a5, a4, a3, a2, a1, b0);
}

yur_Ref *yu__pa_fn0shift_doNondetC1(yur_Ref *a6, yur_Ref *a5, yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn0shift_doNondetC(a6, a5, a4, a3, a2, a1, b0);
}

yur_Ref *yu__pa_fn13timeTestChoice14(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_unref(self);
  return yu__fn13timeTestChoice1(a1, b3, b2, b1, b0);
}

yur_Ref *yu__pa_du_fn1handle_doHandler4(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_unref(self);
  return yu__du_fn1handle_doHandler(a1, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn1run_doStateC1(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn1run_doStateC(a1, b0);
}

yur_Ref *yu__pa_fn13timeTestChoice24(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_unref(self);
  return yu__fn13timeTestChoice2(a1, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn17timeTestChoice15(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_unref(self);
  return yu__fn17timeTestChoice1(a1, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn21timeTestChoice16(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_Ref *b5 = self->fields[5];
  yur_inc(b5);
  yur_unref(self);
  return yu__fn21timeTestChoice1(a1, b5, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn25timeTestChoice17(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_Ref *b5 = self->fields[5];
  yur_inc(b5);
  yur_Ref *b6 = self->fields[6];
  yur_inc(b6);
  yur_unref(self);
  return yu__fn25timeTestChoice1(a1, b6, b5, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn17timeTestChoice24(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_unref(self);
  return yu__fn17timeTestChoice2(a1, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn21timeTestChoice24(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_unref(self);
  return yu__fn21timeTestChoice2(a1, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn25timeTestChoice24(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_unref(self);
  return yu__fn25timeTestChoice2(a1, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn2chooseAll_doNondetC7(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_Ref *b5 = self->fields[5];
  yur_inc(b5);
  yur_Ref *b6 = self->fields[6];
  yur_inc(b6);
  yur_unref(self);
  return yu__fn2chooseAll_doNondetC(a1, b6, b5, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_du_du_fn4chooseAll_doNondetC2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__du_du_fn4chooseAll_doNondetC(a1, b1, b0);
}

yur_Ref *yu__pa_fn0chooseAll_doNondetC6(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_Ref *b5 = self->fields[5];
  yur_inc(b5);
  yur_unref(self);
  return yu__fn0chooseAll_doNondetC(a1, b5, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_du_fn1unit_doStateC3(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_unref(self);
  return yu__du_fn1unit_doStateC(a1, b2, b1, b0);
}

yur_Ref *yu__pa_du_fn2modify_doStateC5(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_unref(self);
  return yu__du_fn2modify_doStateC(a1, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn2runC0___modify_doStateC1(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn2runC0___modify_doStateC(a1, b0);
}

yur_Ref *yu__pa_fn8shift_doStateC1(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn8shift_doStateC(a1, b0);
}

yur_Ref *yu__pa_du_fn5performShift5(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_unref(self);
  return yu__du_fn5performShift(a1, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_du_fn9performShift6(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_Ref *b5 = self->fields[5];
  yur_inc(b5);
  yur_unref(self);
  return yu__du_fn9performShift(a1, b5, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_du_fn14performShift7(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_Ref *b5 = self->fields[5];
  yur_inc(b5);
  yur_Ref *b6 = self->fields[6];
  yur_inc(b6);
  yur_unref(self);
  return yu__du_fn14performShift(a1, b6, b5, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_in_co_gr_baTy1(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__in_co_gr_baTy(a1, b0);
}

yur_Ref *yu__pa_fn0isFunctor_doContT2(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__fn0isFunctor_doContT(a4, a3, a2, a1, b1, b0);
}

yur_Ref *yu__pa_fn1_in_at_ba_in_mi_gr2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__fn1_in_at_ba_in_mi_gr(a1, b1, b0);
}

yur_Ref *yu__pa_fn1isFunctor_doContT1(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn1isFunctor_doContT(a1, b0);
}

yur_Ref *yu__pa_fn1map_doContT6(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_Ref *b5 = self->fields[5];
  yur_inc(b5);
  yur_unref(self);
  return yu__fn1map_doContT(a1, b5, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn2bind_doContT2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__fn2bind_doContT(a1, b1, b0);
}

yur_Ref *yu__pa_du_du_fn2shift_doStateC2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__du_du_fn2shift_doStateC(a1, b1, b0);
}

yur_Ref *yu__pa_fn6shift_doStateC7(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_Ref *b3 = self->fields[3];
  yur_inc(b3);
  yur_Ref *b4 = self->fields[4];
  yur_inc(b4);
  yur_Ref *b5 = self->fields[5];
  yur_inc(b5);
  yur_Ref *b6 = self->fields[6];
  yur_inc(b6);
  yur_unref(self);
  return yu__fn6shift_doStateC(a1, b6, b5, b4, b3, b2, b1, b0);
}

yur_Ref *yu__pa_fn1pure_doContT1(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn1pure_doContT(a1, b0);
}

yur_Ref *yu__pa_fn1bind_doContT2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__fn1bind_doContT(a1, b1, b0);
}

yur_Ref *yu__pa_fn1_poelse_baThen_doBool3(yur_Ref *self) {
  yur_Ref *b0 = self->fields[1];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[2];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[3];
  yur_inc(b2);
  return yu__fn1_poelse_baThen_doBool(b2, b1, b0);
}

yur_Ref yu__im_pa_fn1_poelse_baThen_doBool3 = {
  .count = 0,
  .tag = (size_t) &yu__pa_fn1_poelse_baThen_doBool3,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

yur_Ref *yu__pa_du_fn9modify_doStateC1(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__du_fn9modify_doStateC(a1, b0);
}

yur_Ref *yu__pa_fn4shift_doStateC1(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_unref(self);
  return yu__fn4shift_doStateC(a1, b0);
}

yur_Ref *yu__pa_fn2_poalt_baIn2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__fn2_poalt_baIn(a1, b1, b0);
}

yur_Ref *yu__pa_du_du_fn6modify_doStateC2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__du_du_fn6modify_doStateC(a1, b1, b0);
}

yur_Ref *yu__pa_fn0runC0___modify_doStateC3(yur_Ref *a4, yur_Ref *a3, yur_Ref *a2, yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_Ref *b2 = self->fields[2];
  yur_inc(b2);
  yur_unref(self);
  return yu__fn0runC0___modify_doStateC(a4, a3, a2, a1, b2, b1, b0);
}

yur_Ref *yu__pa_du_du_fn4shift_doNondetC2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__du_du_fn4shift_doNondetC(a1, b1, b0);
}

yur_Ref *yu__pa_du_du_fn2shift_doNondetC2(yur_Ref *a1, yur_Ref *self) {
  yur_Ref *b0 = self->fields[0];
  yur_inc(b0);
  yur_Ref *b1 = self->fields[1];
  yur_inc(b1);
  yur_unref(self);
  return yu__du_du_fn2shift_doNondetC(a1, b1, b0);
}

yur_Ref *yu_testList() {
  yur_Ref *x15683;
  x15683 = yur_ALOAD(yu__im10_doBin.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im10_doBin.tag))) {
    
  } else {
    yur_Ref *expect = x15683;
    x15683 = yu_10_doBin();
    yur_memoize(&yu__im10_doBin, &yu__im10_doBin.fields[0], &x15683, expect);
    yur_ASTORE(yu__im10_doBin.tag, 1);
  }
  yur_inc(x15683);
  yur_Ref *x15684;
  x15684 = yur_ALOAD(yu__im10_doBin.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im10_doBin.tag))) {
    
  } else {
    yur_Ref *expect = x15684;
    x15684 = yu_10_doBin();
    yur_memoize(&yu__im10_doBin, &yu__im10_doBin.fields[0], &x15684, expect);
    yur_ASTORE(yu__im10_doBin.tag, 1);
  }
  yur_inc(x15684);
  yur_Ref *x39;
  x39 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_as_baBin)(x15684, x15683);
  yur_Ref *x17921;
  x17921 = &yur_unit;
  yur_inc(x17921);
  yur_Ref *x20561;
  x20561 = &yur_unit;
  yur_inc(x20561);
  yur_inc(x39);
  yur_Ref *x20559;
  x20559 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__pocmp_baBin)(x20561, x39);
  yur_Ref *x20560;
  x20560 = ((yur_Ref *(*)(yur_Ref *))yu__poeq_qu_baOrder)(x20559);
  yur_Ref *x22420;
  x22420 = yur_build(1, 0);
  x22420->fields[0] = x20560;
  yur_Ref *x17935;
  x17935 = yur_alloc(2);
  yur_init(x17935, 1, 0);
  x17935->fields[0] = (yur_Ref *) &yu__im_pa_fn0makeTestList1;
  yur_inc(x17921);
  x17935->nfields = 2;
  x17935->fields[1] = x17921;
  yur_Ref *x22419;
  x22419 = yur_build(2, 0);
  x22419->fields[0] = x22420;
  x22419->fields[1] = x17935;
  yur_Ref *x17938;
  x17938 = yur_alloc(3);
  yur_init(x17938, 1, 0);
  x17938->fields[0] = (yur_Ref *) &yu__im_pa_fn1makeTestList2;
  x17938->nfields = 3;
  x17938->fields[1] = x39;
  x17938->fields[2] = x17921;
  yur_Ref *x21943;
  x21943 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0_poelse_baThen_doBool)(x17938, x22419);
  return x21943;
}

yur_Ref *yu_timeTestChoice1() {
  yur_Ref *x17941;
  x17941 = &yur_unit;
  yur_inc(x17941);
  yur_Ref *x17942;
  x17942 = &yur_unit;
  yur_inc(x17942);
  yur_Ref *x17943;
  x17943 = &yur_unit;
  yur_inc(x17943);
  yur_Ref *x17950;
  x17950 = &yur_unit;
  yur_inc(x17950);
  yur_Ref *x17945;
  x17945 = &yu__ct1;
  yur_inc(x17945);
  yur_Ref *x17948;
  x17948 = &yur_unit;
  yur_inc(x17948);
  yur_Ref *x22426;
  x22426 = &yur_unit;
  yur_inc(x22426);
  yur_inc(x17945);
  yur_Ref *x20571;
  x20571 = ((yur_Ref *(*)(yur_Ref *))yu__fn0isFunctor_doComp)(x17945);
  yur_inc(x17950);
  yur_Ref *x21947;
  x21947 = yur_build(1, (size_t) &yu__pa_fn0isFunctorEnv_doStateC1);
  x21947->fields[0] = x17950;
  yur_inc(x17950);
  yur_inc(x17941);
  yur_Ref *x21948;
  x21948 = yur_build(2, (size_t) &yu__pa_fn0unit_doStateC2);
  x21948->fields[0] = x17950;
  x21948->fields[1] = x17941;
  yur_inc(x17950);
  yur_inc(x17941);
  yur_Ref *x21949;
  x21949 = yur_build(2, (size_t) &yu__pa_fn0modify_doStateC2);
  x21949->fields[0] = x17950;
  x21949->fields[1] = x17941;
  yur_inc(x17941);
  yur_Ref *x22589;
  x22589 = yur_build(4, 0);
  x22589->fields[0] = x17941;
  x22589->fields[1] = x21947;
  x22589->fields[2] = x21948;
  x22589->fields[3] = x21949;
  yur_inc(x17950);
  yur_Ref *x21953;
  x21953 = yur_build(1, (size_t) &yu__pa_fn0isFunctorEnv_doStateC1);
  x21953->fields[0] = x17950;
  yur_inc(x17950);
  yur_inc(x17941);
  yur_Ref *x21954;
  x21954 = yur_build(2, (size_t) &yu__pa_fn0unit_doStateC2);
  x21954->fields[0] = x17950;
  x21954->fields[1] = x17941;
  yur_inc(x17950);
  yur_inc(x17941);
  yur_Ref *x21955;
  x21955 = yur_build(2, (size_t) &yu__pa_fn0modify_doStateC2);
  x21955->fields[0] = x17950;
  x21955->fields[1] = x17941;
  yur_inc(x17941);
  yur_Ref *x22590;
  x22590 = yur_build(4, 0);
  x22590->fields[0] = x17941;
  x22590->fields[1] = x21953;
  x22590->fields[2] = x21954;
  x22590->fields[3] = x21955;
  yur_inc(x17941);
  yur_Ref *x21958;
  x21958 = yur_build(2, (size_t) &yu__pa_fn0shift_doStateC2);
  x21958->fields[0] = x17950;
  x21958->fields[1] = x17941;
  yur_inc(x17941);
  yur_Ref *x22588;
  x22588 = yur_build(4, 0);
  x22588->fields[0] = x17941;
  x22588->fields[1] = x22590;
  x22588->fields[2] = x21958;
  x22588->fields[3] = &yu__im_fn0reset_doStateC;
  yur_Ref *x22583;
  x22583 = &yur_unit;
  yur_inc(x22583);
  yur_inc(x22589);
  yur_inc(x17942);
  yur_inc(x17945);
  yur_Ref *x22584;
  x22584 = yur_build(5, 0);
  x22584->fields[0] = x17941;
  x22584->fields[1] = x22589;
  x22584->fields[2] = x17942;
  x22584->fields[3] = x22588;
  x22584->fields[4] = x17945;
  yur_Ref *x22592;
  x22592 = &yur_unit;
  yur_inc(x22592);
  yur_Ref *x22593;
  x22593 = &yur_unit;
  yur_inc(x22593);
  yur_Ref *x22594;
  x22594 = yur_build(2, 1);
  x22594->fields[0] = &yu__im_fn0State;
  x22594->fields[1] = x22593;
  yur_Ref *x22595;
  x22595 = &yur_unit;
  yur_inc(x22595);
  yur_inc(x22583);
  yur_Ref *x22596;
  x22596 = yur_build(2, 1);
  x22596->fields[0] = x22583;
  x22596->fields[1] = x22595;
  yur_inc(x22594);
  yur_Ref *x22598;
  x22598 = yur_build(1, (size_t) &yu__pa_fn0unit_doNondetC1);
  x22598->fields[0] = x22594;
  yur_inc(x22594);
  yur_Ref *x22599;
  x22599 = yur_build(1, (size_t) &yu__pa_fn0modify_doNondetC1);
  x22599->fields[0] = x22594;
  yur_inc(x22594);
  yur_Ref *x22781;
  x22781 = yur_build(4, 0);
  x22781->fields[0] = x22594;
  x22781->fields[1] = &yu__imisFunctorEnv_doNondetC;
  x22781->fields[2] = x22598;
  x22781->fields[3] = x22599;
  yur_inc(x22594);
  yur_Ref *x22602;
  x22602 = yur_build(1, (size_t) &yu__pa_fn0unit_doNondetC1);
  x22602->fields[0] = x22594;
  yur_inc(x22594);
  yur_Ref *x22603;
  x22603 = yur_build(1, (size_t) &yu__pa_fn0modify_doNondetC1);
  x22603->fields[0] = x22594;
  yur_inc(x22594);
  yur_Ref *x22782;
  x22782 = yur_build(4, 0);
  x22782->fields[0] = x22594;
  x22782->fields[1] = &yu__imisFunctorEnv_doNondetC;
  x22782->fields[2] = x22602;
  x22782->fields[3] = x22603;
  yur_inc(x22594);
  yur_Ref *x22606;
  x22606 = yur_build(1, (size_t) &yu__pa_fn0shift_doNondetC1);
  x22606->fields[0] = x22594;
  yur_inc(x22594);
  yur_Ref *x22780;
  x22780 = yur_build(4, 0);
  x22780->fields[0] = x22594;
  x22780->fields[1] = x22782;
  x22780->fields[2] = x22606;
  x22780->fields[3] = &yu__im_fn0reset_doNondetC;
  yur_Ref *x22775;
  x22775 = &yur_unit;
  yur_inc(x22775);
  yur_inc(x22781);
  yur_inc(x22596);
  yur_inc(x22584);
  yur_Ref *x22776;
  x22776 = yur_build(5, 0);
  x22776->fields[0] = x22594;
  x22776->fields[1] = x22781;
  x22776->fields[2] = x22596;
  x22776->fields[3] = x22780;
  x22776->fields[4] = x22584;
  yur_Ref *x22783;
  x22783 = &yur_unit;
  yur_inc(x22783);
  yur_Ref *x22784;
  x22784 = yur_build(2, 1);
  x22784->fields[0] = &yu__im_fn0State;
  x22784->fields[1] = x22783;
  yur_Ref *x22785;
  x22785 = yur_build(2, 1);
  x22785->fields[0] = &yu__imNondet;
  x22785->fields[1] = x22784;
  yur_Ref *x22786;
  x22786 = &yur_unit;
  yur_inc(x22786);
  yur_inc(x22583);
  yur_Ref *x22787;
  x22787 = yur_build(2, 1);
  x22787->fields[0] = x22583;
  x22787->fields[1] = x22786;
  yur_inc(x22775);
  yur_Ref *x22788;
  x22788 = yur_build(2, 1);
  x22788->fields[0] = x22775;
  x22788->fields[1] = x22787;
  yur_Ref *x22789;
  x22789 = &yur_unit;
  yur_inc(x22789);
  yur_Ref *x22790;
  x22790 = &yur_unit;
  yur_inc(x22790);
  yur_Ref *x22791;
  x22791 = &yur_unit;
  yur_inc(x22791);
  yur_Ref *x22792;
  x22792 = &yur_unit;
  yur_inc(x22792);
  yur_inc(x22776);
  yur_Ref *x22793;
  x22793 = ((yur_Ref *(*)(yur_Ref *))yu__fn0isFunctor_doComp)(x22776);
  yur_inc(x22790);
  yur_Ref *x22794;
  x22794 = yur_build(1, (size_t) &yu__pa_fn0isFunctorEnv_doStateC1);
  x22794->fields[0] = x22790;
  yur_inc(x22790);
  yur_inc(x22785);
  yur_Ref *x22795;
  x22795 = yur_build(2, (size_t) &yu__pa_fn0unit_doStateC2);
  x22795->fields[0] = x22790;
  x22795->fields[1] = x22785;
  yur_inc(x22790);
  yur_inc(x22785);
  yur_Ref *x22796;
  x22796 = yur_build(2, (size_t) &yu__pa_fn0modify_doStateC2);
  x22796->fields[0] = x22790;
  x22796->fields[1] = x22785;
  yur_inc(x22785);
  yur_Ref *x22797;
  x22797 = yur_build(4, 0);
  x22797->fields[0] = x22785;
  x22797->fields[1] = x22794;
  x22797->fields[2] = x22795;
  x22797->fields[3] = x22796;
  yur_inc(x22790);
  yur_Ref *x22798;
  x22798 = yur_build(1, (size_t) &yu__pa_fn0isFunctorEnv_doStateC1);
  x22798->fields[0] = x22790;
  yur_inc(x22790);
  yur_inc(x22785);
  yur_Ref *x22799;
  x22799 = yur_build(2, (size_t) &yu__pa_fn0unit_doStateC2);
  x22799->fields[0] = x22790;
  x22799->fields[1] = x22785;
  yur_inc(x22790);
  yur_inc(x22785);
  yur_Ref *x22800;
  x22800 = yur_build(2, (size_t) &yu__pa_fn0modify_doStateC2);
  x22800->fields[0] = x22790;
  x22800->fields[1] = x22785;
  yur_inc(x22785);
  yur_Ref *x22801;
  x22801 = yur_build(4, 0);
  x22801->fields[0] = x22785;
  x22801->fields[1] = x22798;
  x22801->fields[2] = x22799;
  x22801->fields[3] = x22800;
  yur_inc(x22785);
  yur_Ref *x22802;
  x22802 = yur_build(2, (size_t) &yu__pa_fn0shift_doStateC2);
  x22802->fields[0] = x22790;
  x22802->fields[1] = x22785;
  yur_inc(x22785);
  yur_Ref *x22803;
  x22803 = yur_build(4, 0);
  x22803->fields[0] = x22785;
  x22803->fields[1] = x22801;
  x22803->fields[2] = x22802;
  x22803->fields[3] = &yu__im_fn0reset_doStateC;
  yur_Ref *x22804;
  x22804 = &yur_unit;
  yur_inc(x22804);
  yur_inc(x22797);
  yur_inc(x22788);
  yur_inc(x22776);
  yur_Ref *x22805;
  x22805 = yur_build(5, 0);
  x22805->fields[0] = x22785;
  x22805->fields[1] = x22797;
  x22805->fields[2] = x22788;
  x22805->fields[3] = x22803;
  x22805->fields[4] = x22776;
  yur_inc(x22805);
  yur_Ref *x22806;
  x22806 = yur_build(4, (size_t) &yu__pa_fn13timeTestChoice14);
  x22806->fields[0] = x22583;
  x22806->fields[1] = x22775;
  x22806->fields[2] = x22804;
  x22806->fields[3] = x22805;
  yur_Ref *x22807;
  x22807 = &yur_unit;
  yur_inc(x22807);
  yur_Ref *x22808;
  x22808 = &yur_unit;
  yur_inc(x22808);
  yur_Ref *x22809;
  x22809 = yur_build(2, 1);
  x22809->fields[0] = &yu__im_fn0State;
  x22809->fields[1] = x22808;
  yur_Ref *x22810;
  x22810 = yur_build(2, 1);
  x22810->fields[0] = &yu__imNondet;
  x22810->fields[1] = x22809;
  yur_Ref *x22811;
  x22811 = &yur_unit;
  yur_inc(x22811);
  yur_Ref *x22812;
  x22812 = yur_build(2, 1);
  x22812->fields[0] = &yu__im_fn0State;
  x22812->fields[1] = x22811;
  yur_Ref *x22813;
  x22813 = yur_build(1, 1);
  x22813->fields[0] = x22812;
  yur_Ref *x22814;
  x22814 = yur_build(2, 0);
  x22814->fields[0] = x22810;
  x22814->fields[1] = x22813;
  yur_Ref *x22815;
  x22815 = yur_ALOAD(yu__imtestList.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__imtestList.tag))) {
    
  } else {
    yur_Ref *expect = x22815;
    x22815 = yu_testList();
    yur_memoize(&yu__imtestList, &yu__imtestList.fields[0], &x22815, expect);
    yur_ASTORE(yu__imtestList.tag, 1);
  }
  yur_inc(x22815);
  yur_inc(x22805);
  yur_Ref *x22816;
  x22816 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pochoose_baIn)(x22815, x22805, x22814, x22807);
  yur_Ref *x22817;
  x22817 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x22806, x22816, x22805);
  yur_inc(x22789);
  yur_inc(x22776);
  yur_Ref *x22818;
  x22818 = yur_build(4, (size_t) &yu__pa_du_fn1handle_doHandler4);
  x22818->fields[0] = x22789;
  x22818->fields[1] = x22788;
  x22818->fields[2] = x22797;
  x22818->fields[3] = x22776;
  yur_Ref *x22819;
  x22819 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x22817->tag)(x22818, x22817);
  yur_Ref *x22820;
  x22820 = yur_build(1, (size_t) &yu__pa_fn1run_doStateC1);
  x22820->fields[0] = x22791;
  yur_Ref *x22821;
  x22821 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x22820, x22819, x22776);
  yur_Ref *x22822;
  x22822 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *)) x22793->tag)(x22789, x22792, &yu__im_fn0_po2_ba_pr_am_ba_in_mi_gr, x22821, x22793);
  yur_Ref *x22778;
  x22778 = yur_build(4, (size_t) &yu__pa_du_fn1handle_doHandler4);
  x22778->fields[0] = x22592;
  x22778->fields[1] = x22596;
  x22778->fields[2] = x22781;
  x22778->fields[3] = x22584;
  yur_Ref *x22779;
  x22779 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x22822->tag)(x22778, x22822);
  yur_inc(x17943);
  yur_inc(x17945);
  yur_Ref *x22586;
  x22586 = yur_build(4, (size_t) &yu__pa_du_fn1handle_doHandler4);
  x22586->fields[0] = x17943;
  x22586->fields[1] = x17942;
  x22586->fields[2] = x22589;
  x22586->fields[3] = x17945;
  yur_Ref *x22587;
  x22587 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x22779->tag)(x22586, x22779);
  yur_Ref *x21962;
  x21962 = yur_build(1, (size_t) &yu__pa_fn1run_doStateC1);
  x21962->fields[0] = x17948;
  yur_Ref *x21963;
  x21963 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21962, x22587, x17945);
  yur_Ref *x21944;
  x21944 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *)) x20571->tag)(x17943, x22426, &yu__im_fn0_po2_ba_pr_am_ba_in_mi_gr, x21963, x20571);
  return x21944;
}

yur_Ref *yu_timeTestChoice2() {
  yur_Ref *x17951;
  x17951 = &yur_unit;
  yur_inc(x17951);
  yur_Ref *x17952;
  x17952 = &yur_unit;
  yur_inc(x17952);
  yur_Ref *x17953;
  x17953 = &yur_unit;
  yur_inc(x17953);
  yur_Ref *x17960;
  x17960 = &yur_unit;
  yur_inc(x17960);
  yur_Ref *x17955;
  x17955 = &yu__ct1;
  yur_inc(x17955);
  yur_Ref *x17958;
  x17958 = &yur_unit;
  yur_inc(x17958);
  yur_Ref *x22432;
  x22432 = &yur_unit;
  yur_inc(x22432);
  yur_inc(x17955);
  yur_Ref *x20582;
  x20582 = ((yur_Ref *(*)(yur_Ref *))yu__fn0isFunctor_doComp)(x17955);
  yur_inc(x17960);
  yur_Ref *x21968;
  x21968 = yur_build(1, (size_t) &yu__pa_fn0isFunctorEnv_doStateC1);
  x21968->fields[0] = x17960;
  yur_inc(x17960);
  yur_inc(x17951);
  yur_Ref *x21969;
  x21969 = yur_build(2, (size_t) &yu__pa_fn0unit_doStateC2);
  x21969->fields[0] = x17960;
  x21969->fields[1] = x17951;
  yur_inc(x17960);
  yur_inc(x17951);
  yur_Ref *x21970;
  x21970 = yur_build(2, (size_t) &yu__pa_fn0modify_doStateC2);
  x21970->fields[0] = x17960;
  x21970->fields[1] = x17951;
  yur_inc(x17951);
  yur_Ref *x22615;
  x22615 = yur_build(4, 0);
  x22615->fields[0] = x17951;
  x22615->fields[1] = x21968;
  x22615->fields[2] = x21969;
  x22615->fields[3] = x21970;
  yur_inc(x17960);
  yur_Ref *x21974;
  x21974 = yur_build(1, (size_t) &yu__pa_fn0isFunctorEnv_doStateC1);
  x21974->fields[0] = x17960;
  yur_inc(x17960);
  yur_inc(x17951);
  yur_Ref *x21975;
  x21975 = yur_build(2, (size_t) &yu__pa_fn0unit_doStateC2);
  x21975->fields[0] = x17960;
  x21975->fields[1] = x17951;
  yur_inc(x17960);
  yur_inc(x17951);
  yur_Ref *x21976;
  x21976 = yur_build(2, (size_t) &yu__pa_fn0modify_doStateC2);
  x21976->fields[0] = x17960;
  x21976->fields[1] = x17951;
  yur_inc(x17951);
  yur_Ref *x22616;
  x22616 = yur_build(4, 0);
  x22616->fields[0] = x17951;
  x22616->fields[1] = x21974;
  x22616->fields[2] = x21975;
  x22616->fields[3] = x21976;
  yur_inc(x17951);
  yur_Ref *x21979;
  x21979 = yur_build(2, (size_t) &yu__pa_fn0shift_doStateC2);
  x21979->fields[0] = x17960;
  x21979->fields[1] = x17951;
  yur_inc(x17951);
  yur_Ref *x22614;
  x22614 = yur_build(4, 0);
  x22614->fields[0] = x17951;
  x22614->fields[1] = x22616;
  x22614->fields[2] = x21979;
  x22614->fields[3] = &yu__im_fn0reset_doStateC;
  yur_Ref *x22609;
  x22609 = &yur_unit;
  yur_inc(x22609);
  yur_inc(x22615);
  yur_inc(x17952);
  yur_inc(x17955);
  yur_Ref *x22610;
  x22610 = yur_build(5, 0);
  x22610->fields[0] = x17951;
  x22610->fields[1] = x22615;
  x22610->fields[2] = x17952;
  x22610->fields[3] = x22614;
  x22610->fields[4] = x17955;
  yur_Ref *x22618;
  x22618 = &yur_unit;
  yur_inc(x22618);
  yur_Ref *x22619;
  x22619 = &yur_unit;
  yur_inc(x22619);
  yur_Ref *x22620;
  x22620 = yur_build(2, 1);
  x22620->fields[0] = &yu__im_fn0State;
  x22620->fields[1] = x22619;
  yur_Ref *x22621;
  x22621 = &yur_unit;
  yur_inc(x22621);
  yur_inc(x22609);
  yur_Ref *x22622;
  x22622 = yur_build(2, 1);
  x22622->fields[0] = x22609;
  x22622->fields[1] = x22621;
  yur_inc(x22620);
  yur_Ref *x22624;
  x22624 = yur_build(1, (size_t) &yu__pa_fn0unit_doNondetC1);
  x22624->fields[0] = x22620;
  yur_inc(x22620);
  yur_Ref *x22625;
  x22625 = yur_build(1, (size_t) &yu__pa_fn0modify_doNondetC1);
  x22625->fields[0] = x22620;
  yur_inc(x22620);
  yur_Ref *x22829;
  x22829 = yur_build(4, 0);
  x22829->fields[0] = x22620;
  x22829->fields[1] = &yu__imisFunctorEnv_doNondetC;
  x22829->fields[2] = x22624;
  x22829->fields[3] = x22625;
  yur_inc(x22620);
  yur_Ref *x22628;
  x22628 = yur_build(1, (size_t) &yu__pa_fn0unit_doNondetC1);
  x22628->fields[0] = x22620;
  yur_inc(x22620);
  yur_Ref *x22629;
  x22629 = yur_build(1, (size_t) &yu__pa_fn0modify_doNondetC1);
  x22629->fields[0] = x22620;
  yur_inc(x22620);
  yur_Ref *x22830;
  x22830 = yur_build(4, 0);
  x22830->fields[0] = x22620;
  x22830->fields[1] = &yu__imisFunctorEnv_doNondetC;
  x22830->fields[2] = x22628;
  x22830->fields[3] = x22629;
  yur_inc(x22620);
  yur_Ref *x22632;
  x22632 = yur_build(1, (size_t) &yu__pa_fn0shift_doNondetC1);
  x22632->fields[0] = x22620;
  yur_inc(x22620);
  yur_Ref *x22828;
  x22828 = yur_build(4, 0);
  x22828->fields[0] = x22620;
  x22828->fields[1] = x22830;
  x22828->fields[2] = x22632;
  x22828->fields[3] = &yu__im_fn0reset_doNondetC;
  yur_Ref *x22823;
  x22823 = &yur_unit;
  yur_inc(x22823);
  yur_inc(x22829);
  yur_inc(x22622);
  yur_inc(x22610);
  yur_Ref *x22824;
  x22824 = yur_build(5, 0);
  x22824->fields[0] = x22620;
  x22824->fields[1] = x22829;
  x22824->fields[2] = x22622;
  x22824->fields[3] = x22828;
  x22824->fields[4] = x22610;
  yur_Ref *x22831;
  x22831 = &yur_unit;
  yur_inc(x22831);
  yur_Ref *x22832;
  x22832 = yur_build(2, 1);
  x22832->fields[0] = &yu__im_fn0State;
  x22832->fields[1] = x22831;
  yur_Ref *x22833;
  x22833 = yur_build(2, 1);
  x22833->fields[0] = &yu__imNondet;
  x22833->fields[1] = x22832;
  yur_Ref *x22834;
  x22834 = &yur_unit;
  yur_inc(x22834);
  yur_inc(x22609);
  yur_Ref *x22835;
  x22835 = yur_build(2, 1);
  x22835->fields[0] = x22609;
  x22835->fields[1] = x22834;
  yur_inc(x22823);
  yur_Ref *x22836;
  x22836 = yur_build(2, 1);
  x22836->fields[0] = x22823;
  x22836->fields[1] = x22835;
  yur_Ref *x22837;
  x22837 = &yur_unit;
  yur_inc(x22837);
  yur_Ref *x22838;
  x22838 = &yur_unit;
  yur_inc(x22838);
  yur_Ref *x22839;
  x22839 = &yur_unit;
  yur_inc(x22839);
  yur_Ref *x22840;
  x22840 = &yur_unit;
  yur_inc(x22840);
  yur_inc(x22824);
  yur_Ref *x22841;
  x22841 = ((yur_Ref *(*)(yur_Ref *))yu__fn0isFunctor_doComp)(x22824);
  yur_inc(x22838);
  yur_Ref *x22842;
  x22842 = yur_build(1, (size_t) &yu__pa_fn0isFunctorEnv_doStateC1);
  x22842->fields[0] = x22838;
  yur_inc(x22838);
  yur_inc(x22833);
  yur_Ref *x22843;
  x22843 = yur_build(2, (size_t) &yu__pa_fn0unit_doStateC2);
  x22843->fields[0] = x22838;
  x22843->fields[1] = x22833;
  yur_inc(x22838);
  yur_inc(x22833);
  yur_Ref *x22844;
  x22844 = yur_build(2, (size_t) &yu__pa_fn0modify_doStateC2);
  x22844->fields[0] = x22838;
  x22844->fields[1] = x22833;
  yur_inc(x22833);
  yur_Ref *x22845;
  x22845 = yur_build(4, 0);
  x22845->fields[0] = x22833;
  x22845->fields[1] = x22842;
  x22845->fields[2] = x22843;
  x22845->fields[3] = x22844;
  yur_inc(x22838);
  yur_Ref *x22846;
  x22846 = yur_build(1, (size_t) &yu__pa_fn0isFunctorEnv_doStateC1);
  x22846->fields[0] = x22838;
  yur_inc(x22838);
  yur_inc(x22833);
  yur_Ref *x22847;
  x22847 = yur_build(2, (size_t) &yu__pa_fn0unit_doStateC2);
  x22847->fields[0] = x22838;
  x22847->fields[1] = x22833;
  yur_inc(x22838);
  yur_inc(x22833);
  yur_Ref *x22848;
  x22848 = yur_build(2, (size_t) &yu__pa_fn0modify_doStateC2);
  x22848->fields[0] = x22838;
  x22848->fields[1] = x22833;
  yur_inc(x22833);
  yur_Ref *x22849;
  x22849 = yur_build(4, 0);
  x22849->fields[0] = x22833;
  x22849->fields[1] = x22846;
  x22849->fields[2] = x22847;
  x22849->fields[3] = x22848;
  yur_inc(x22833);
  yur_Ref *x22850;
  x22850 = yur_build(2, (size_t) &yu__pa_fn0shift_doStateC2);
  x22850->fields[0] = x22838;
  x22850->fields[1] = x22833;
  yur_inc(x22833);
  yur_Ref *x22851;
  x22851 = yur_build(4, 0);
  x22851->fields[0] = x22833;
  x22851->fields[1] = x22849;
  x22851->fields[2] = x22850;
  x22851->fields[3] = &yu__im_fn0reset_doStateC;
  yur_Ref *x22852;
  x22852 = &yur_unit;
  yur_inc(x22852);
  yur_inc(x22845);
  yur_inc(x22836);
  yur_inc(x22824);
  yur_Ref *x22853;
  x22853 = yur_build(5, 0);
  x22853->fields[0] = x22833;
  x22853->fields[1] = x22845;
  x22853->fields[2] = x22836;
  x22853->fields[3] = x22851;
  x22853->fields[4] = x22824;
  yur_inc(x22853);
  yur_Ref *x22854;
  x22854 = yur_build(4, (size_t) &yu__pa_fn13timeTestChoice24);
  x22854->fields[0] = x22609;
  x22854->fields[1] = x22823;
  x22854->fields[2] = x22852;
  x22854->fields[3] = x22853;
  yur_Ref *x22855;
  x22855 = &yur_unit;
  yur_inc(x22855);
  yur_Ref *x22856;
  x22856 = &yur_unit;
  yur_inc(x22856);
  yur_Ref *x22857;
  x22857 = yur_build(2, 1);
  x22857->fields[0] = &yu__im_fn0State;
  x22857->fields[1] = x22856;
  yur_Ref *x22858;
  x22858 = yur_build(2, 1);
  x22858->fields[0] = &yu__imNondet;
  x22858->fields[1] = x22857;
  yur_Ref *x22859;
  x22859 = &yur_unit;
  yur_inc(x22859);
  yur_Ref *x22860;
  x22860 = yur_build(2, 1);
  x22860->fields[0] = &yu__im_fn0State;
  x22860->fields[1] = x22859;
  yur_Ref *x22861;
  x22861 = yur_build(1, 1);
  x22861->fields[0] = x22860;
  yur_Ref *x22862;
  x22862 = yur_build(2, 0);
  x22862->fields[0] = x22858;
  x22862->fields[1] = x22861;
  yur_Ref *x22863;
  x22863 = yur_ALOAD(yu__imtestList.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__imtestList.tag))) {
    
  } else {
    yur_Ref *expect = x22863;
    x22863 = yu_testList();
    yur_memoize(&yu__imtestList, &yu__imtestList.fields[0], &x22863, expect);
    yur_ASTORE(yu__imtestList.tag, 1);
  }
  yur_inc(x22863);
  yur_inc(x22853);
  yur_Ref *x22864;
  x22864 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pochoose_baIn)(x22863, x22853, x22862, x22855);
  yur_Ref *x22865;
  x22865 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x22854, x22864, x22853);
  yur_inc(x22837);
  yur_inc(x22824);
  yur_Ref *x22866;
  x22866 = yur_build(4, (size_t) &yu__pa_du_fn1handle_doHandler4);
  x22866->fields[0] = x22837;
  x22866->fields[1] = x22836;
  x22866->fields[2] = x22845;
  x22866->fields[3] = x22824;
  yur_Ref *x22867;
  x22867 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x22865->tag)(x22866, x22865);
  yur_Ref *x22868;
  x22868 = yur_build(1, (size_t) &yu__pa_fn1run_doStateC1);
  x22868->fields[0] = x22839;
  yur_Ref *x22869;
  x22869 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x22868, x22867, x22824);
  yur_Ref *x22870;
  x22870 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *)) x22841->tag)(x22837, x22840, &yu__im_fn0_po2_ba_pr_am_ba_in_mi_gr, x22869, x22841);
  yur_Ref *x22826;
  x22826 = yur_build(4, (size_t) &yu__pa_du_fn1handle_doHandler4);
  x22826->fields[0] = x22618;
  x22826->fields[1] = x22622;
  x22826->fields[2] = x22829;
  x22826->fields[3] = x22610;
  yur_Ref *x22827;
  x22827 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x22870->tag)(x22826, x22870);
  yur_inc(x17953);
  yur_inc(x17955);
  yur_Ref *x22612;
  x22612 = yur_build(4, (size_t) &yu__pa_du_fn1handle_doHandler4);
  x22612->fields[0] = x17953;
  x22612->fields[1] = x17952;
  x22612->fields[2] = x22615;
  x22612->fields[3] = x17955;
  yur_Ref *x22613;
  x22613 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x22827->tag)(x22612, x22827);
  yur_Ref *x21983;
  x21983 = yur_build(1, (size_t) &yu__pa_fn1run_doStateC1);
  x21983->fields[0] = x17958;
  yur_Ref *x21984;
  x21984 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21983, x22613, x17955);
  yur_Ref *x21965;
  x21965 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *)) x20582->tag)(x17953, x22432, &yu__im_fn0_po2_ba_pr_am_ba_in_mi_gr, x21984, x20582);
  return x21965;
}

yur_Ref *yu_main() {
  yur_Ref *x15701;
  x15701 = yur_ALOAD(yu__imtimeTestChoice1.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__imtimeTestChoice1.tag))) {
    
  } else {
    yur_Ref *expect = x15701;
    x15701 = yu_timeTestChoice1();
    yur_memoize(&yu__imtimeTestChoice1, &yu__imtimeTestChoice1.fields[0], &x15701, expect);
    yur_ASTORE(yu__imtimeTestChoice1.tag, 1);
  }
  yur_inc(x15701);
  yur_Ref *x20587;
  x20587 = ((yur_Ref *(*)(yur_Ref *))yu__du_fn0_polen_ba_poList_baTy)(x15701);
  yur_Ref *x1588;
  x1588 = ((yur_Ref *(*)(yur_Ref *))yu__postr_baNat)(x20587);
  yur_Ref *x20599;
  x20599 = (x1588)->fields[0];
  yur_inc(x20599);
  yur_unref(x1588);
  yur_Ref *x20600;
  x20600 = ((yur_Ref *(*)(yur_Ref *))yu_aux0____poprint_baStr)(x20599);
  yur_unref(x20600);
  yur_Ref *x20597;
  x20597 = &yu__ct63;
  yur_inc(x20597);
  yur_Ref *x17985;
  x17985 = ((yur_Ref *(*)(yur_Ref *))yu__poprint_baChar)(x20597);
  yur_unref(x17985);
  yur_Ref *x15705;
  x15705 = yur_ALOAD(yu__imtimeTestChoice2.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__imtimeTestChoice2.tag))) {
    
  } else {
    yur_Ref *expect = x15705;
    x15705 = yu_timeTestChoice2();
    yur_memoize(&yu__imtimeTestChoice2, &yu__imtimeTestChoice2.fields[0], &x15705, expect);
    yur_ASTORE(yu__imtimeTestChoice2.tag, 1);
  }
  yur_inc(x15705);
  yur_Ref *x20588;
  x20588 = ((yur_Ref *(*)(yur_Ref *))yu__du_fn0_polen_ba_poList_baTy)(x15705);
  yur_Ref *x1602;
  x1602 = ((yur_Ref *(*)(yur_Ref *))yu__postr_baNat)(x20588);
  yur_Ref *x20601;
  x20601 = (x1602)->fields[0];
  yur_inc(x20601);
  yur_unref(x1602);
  yur_Ref *x20602;
  x20602 = ((yur_Ref *(*)(yur_Ref *))yu_aux0____poprint_baStr)(x20601);
  yur_unref(x20602);
  yur_Ref *x20598;
  x20598 = &yu__ct63;
  yur_inc(x20598);
  yur_Ref *x17988;
  x17988 = ((yur_Ref *(*)(yur_Ref *))yu__poprint_baChar)(x20598);
  return x17988;
}

yur_Ref *yu_1_doBin() {
  yur_Ref *x17995;
  x17995 = &yur_unit;
  yur_inc(x17995);
  yur_Ref *x17996;
  x17996 = yur_build(1, 1);
  x17996->fields[0] = x17995;
  return x17996;
}

yur_Ref *yu_10_doBin() {
  yur_Ref *x15715;
  x15715 = yur_ALOAD(yu__im10_doBin_pl.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im10_doBin_pl.tag))) {
    
  } else {
    yur_Ref *expect = x15715;
    x15715 = yu_10_doBin_pl();
    yur_memoize(&yu__im10_doBin_pl, &yu__im10_doBin_pl.fields[0], &x15715, expect);
    yur_ASTORE(yu__im10_doBin_pl.tag, 1);
  }
  yur_inc(x15715);
  yur_Ref *x17997;
  x17997 = yur_build(1, 1);
  x17997->fields[0] = x15715;
  return x17997;
}

yur_Ref *yu__pr_mi_mi_mi_baBin_pl(yur_Ref *x1645) {
  switch (x1645->tag) {
  case 0: {
    yur_unref(x1645);
    yur_Ref *x17999;
    x17999 = &yur_unit;
    yur_inc(x17999);
    return x17999;
  }
  case 1: {
    yur_Ref *x1647;
    x1647 = ((yur_Ref *(*)(yur_Ref *))yu__pr_mi_mi_baBin_pl)(x1645);
    yur_Ref *x17998;
    x17998 = yur_build(1, 1);
    x17998->fields[0] = x1647;
    return x17998;
  }
  default:
    yur_panic("unhandled case %zu", x1645->tag);
  }
}

yur_Ref *yu__pr_mi_mi_baBin(yur_Ref *x1650) {
  switch (x1650->tag) {
  case 0: {
    yur_unref(x1650);
    yur_Ref *x18000;
    x18000 = &yur_unit;
    yur_inc(x18000);
    return x18000;
  }
  case 1: {
    yur_Ref *x1651;
    x1651 = (x1650)->fields[0];
    yur_inc(x1651);
    yur_unref(x1650);
    yur_Ref *x14304;
    x14304 = ((yur_Ref *(*)(yur_Ref *))yu__pr_mi_mi_mi_baBin_pl)(x1651);
    return x14304;
  }
  default:
    yur_panic("unhandled case %zu", x1650->tag);
  }
}

yur_Ref *yu__in_as_baBin(yur_Ref *x1664, yur_Ref *x1663) {
  switch (x1664->tag) {
  case 0: {
    yur_unref(x1663);
    yur_unref(x1664);
    yur_Ref *x18003;
    x18003 = &yur_unit;
    yur_inc(x18003);
    return x18003;
  }
  case 1: {
    yur_Ref *x1665;
    x1665 = (x1664)->fields[0];
    yur_inc(x1665);
    yur_unref(x1664);
    switch (x1663->tag) {
    case 0: {
      yur_unref(x1663);
      yur_unref(x1665);
      yur_Ref *x18002;
      x18002 = &yur_unit;
      yur_inc(x18002);
      return x18002;
    }
    case 1: {
      yur_Ref *x1666;
      x1666 = (x1663)->fields[0];
      yur_inc(x1666);
      x1663 = yur_reset(x1663, 1);
      yur_Ref *x1668;
      x1668 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_as_baBin_pl)(x1665, x1666);
      yur_Ref *x18001;
      x18001 = x1663;
      x18001->tag = 1;
      x18001->fields[0] = x1668;
      return x18001;
    }
    default:
      yur_panic("unhandled case %zu", x1663->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x1664->tag);
  }
}

yur_Ref *yu__pocmp_baBin(yur_Ref *x1747, yur_Ref *x1746) {
  switch (x1746->tag) {
  case 0: {
    yur_unref(x1746);
    switch (x1747->tag) {
    case 0: {
      yur_unref(x1747);
      yur_Ref *x18005;
      x18005 = &yu__ct1;
      yur_inc(x18005);
      return x18005;
    }
    case 1: {
      yur_unref(x1747);
      yur_Ref *x18006;
      x18006 = &yur_unit;
      yur_inc(x18006);
      return x18006;
    }
    default:
      yur_panic("unhandled case %zu", x1747->tag);
    }
  }
  case 1: {
    yur_Ref *x1749;
    x1749 = (x1746)->fields[0];
    yur_inc(x1749);
    yur_unref(x1746);
    switch (x1747->tag) {
    case 0: {
      yur_unref(x1747);
      yur_unref(x1749);
      yur_Ref *x18004;
      x18004 = &yu__ct2;
      yur_inc(x18004);
      return x18004;
    }
    case 1: {
      yur_Ref *x1750;
      x1750 = (x1747)->fields[0];
      yur_inc(x1750);
      yur_unref(x1747);
      yur_Ref *x14317;
      x14317 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__pocmp_baBin_pl)(x1750, x1749);
      return x14317;
    }
    default:
      yur_panic("unhandled case %zu", x1747->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x1746->tag);
  }
}

yur_Ref *yu_2_doBin_pl() {
  yur_Ref *x18009;
  x18009 = &yur_unit;
  yur_inc(x18009);
  yur_Ref *x18007;
  x18007 = &yur_unit;
  yur_inc(x18007);
  yur_Ref *x18008;
  x18008 = yur_build(2, 1);
  x18008->fields[0] = x18009;
  x18008->fields[1] = x18007;
  return x18008;
}

yur_Ref *yu_10_doBin_pl() {
  yur_Ref *x18016;
  x18016 = &yur_unit;
  yur_inc(x18016);
  yur_Ref *x18011;
  x18011 = &yur_unit;
  yur_inc(x18011);
  yur_Ref *x18013;
  x18013 = yur_build(2, 1);
  x18013->fields[0] = x18016;
  x18013->fields[1] = x18011;
  yur_Ref *x18010;
  x18010 = &yu__ct1;
  yur_inc(x18010);
  yur_Ref *x18014;
  x18014 = yur_build(2, 1);
  x18014->fields[0] = x18013;
  x18014->fields[1] = x18010;
  yur_Ref *x18012;
  x18012 = &yur_unit;
  yur_inc(x18012);
  yur_Ref *x18015;
  x18015 = yur_build(2, 1);
  x18015->fields[0] = x18014;
  x18015->fields[1] = x18012;
  return x18015;
}

yur_Ref *yu__pr_pl_pl_baBin_pl(yur_Ref *x2202) {
  switch (x2202->tag) {
  case 0: {
    yur_unref(x2202);
    yur_Ref *x15807;
    x15807 = yur_ALOAD(yu__im2_doBin_pl.fields[0]);
    if (yur_LIKELY(yur_ALOAD(yu__im2_doBin_pl.tag))) {
      
    } else {
      yur_Ref *expect = x15807;
      x15807 = yu_2_doBin_pl();
      yur_memoize(&yu__im2_doBin_pl, &yu__im2_doBin_pl.fields[0], &x15807, expect);
      yur_ASTORE(yu__im2_doBin_pl.tag, 1);
    }
    yur_inc(x15807);
    return x15807;
  }
  case 1: {
    yur_Ref *x2203;
    x2203 = (x2202)->fields[0];
    yur_inc(x2203);
    yur_Ref *x2204;
    x2204 = (x2202)->fields[1];
    yur_inc(x2204);
    switch (x2204->tag) {
    case 0: {
      yur_unref(x2204);
      x2202 = yur_reset(x2202, 2);
      yur_Ref *x18017;
      x18017 = &yu__ct1;
      yur_inc(x18017);
      yur_Ref *x18020;
      x18020 = x2202;
      x18020->tag = 1;
      x18020->fields[0] = x2203;
      x18020->fields[1] = x18017;
      return x18020;
    }
    case 1: {
      yur_unref(x2204);
      x2202 = yur_reset(x2202, 2);
      yur_Ref *x2210;
      x2210 = ((yur_Ref *(*)(yur_Ref *))yu__pr_pl_pl_baBin_pl)(x2203);
      yur_Ref *x18018;
      x18018 = &yur_unit;
      yur_inc(x18018);
      yur_Ref *x18019;
      x18019 = x2202;
      x18019->tag = 1;
      x18019->fields[0] = x2210;
      x18019->fields[1] = x18018;
      return x18019;
    }
    default:
      yur_panic("unhandled case %zu", x2204->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x2202->tag);
  }
}

yur_Ref *yu__in_pl_baBin_pl(yur_Ref *x2214, yur_Ref *x2213) {
  switch (x2213->tag) {
  case 0: {
    yur_unref(x2213);
    yur_Ref *x14380;
    x14380 = ((yur_Ref *(*)(yur_Ref *))yu__pr_pl_pl_baBin_pl)(x2214);
    return x14380;
  }
  case 1: {
    yur_Ref *x2217;
    x2217 = (x2213)->fields[0];
    yur_inc(x2217);
    yur_Ref *x2218;
    x2218 = (x2213)->fields[1];
    yur_inc(x2218);
    switch (x2214->tag) {
    case 0: {
      yur_unref(x2214);
      yur_unref(x2217);
      yur_unref(x2218);
      yur_Ref *x14381;
      x14381 = ((yur_Ref *(*)(yur_Ref *))yu__pr_pl_pl_baBin_pl)(x2213);
      return x14381;
    }
    case 1: {
      yur_unref(x2213);
      yur_Ref *x2221;
      x2221 = (x2214)->fields[0];
      yur_inc(x2221);
      yur_Ref *x2222;
      x2222 = (x2214)->fields[1];
      yur_inc(x2222);
      switch (x2222->tag) {
      case 0: {
        yur_unref(x2222);
        x2214 = yur_reset(x2214, 2);
        yur_Ref *x2225;
        x2225 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_pl_baBin_pl)(x2221, x2217);
        yur_Ref *x18024;
        x18024 = x2214;
        x18024->tag = 1;
        x18024->fields[0] = x2225;
        x18024->fields[1] = x2218;
        return x18024;
      }
      case 1: {
        switch (x2218->tag) {
        case 0: {
          yur_unref(x2218);
          x2214 = yur_reset(x2214, 2);
          yur_Ref *x2231;
          x2231 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_pl_baBin_pl)(x2221, x2217);
          yur_Ref *x18023;
          x18023 = x2214;
          x18023->tag = 1;
          x18023->fields[0] = x2231;
          x18023->fields[1] = x2222;
          return x18023;
        }
        case 1: {
          yur_unref(x2218);
          yur_unref(x2222);
          x2214 = yur_reset(x2214, 2);
          yur_Ref *x2240;
          x2240 = ((yur_Ref *(*)(yur_Ref *))yu__pr_pl_pl_baBin_pl)(x2217);
          yur_Ref *x2237;
          x2237 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_pl_baBin_pl)(x2221, x2240);
          yur_Ref *x18021;
          x18021 = &yur_unit;
          yur_inc(x18021);
          yur_Ref *x18022;
          x18022 = x2214;
          x18022->tag = 1;
          x18022->fields[0] = x2237;
          x18022->fields[1] = x18021;
          return x18022;
        }
        default:
          yur_panic("unhandled case %zu", x2218->tag);
        }
      }
      default:
        yur_panic("unhandled case %zu", x2222->tag);
      }
    }
    default:
      yur_panic("unhandled case %zu", x2214->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x2213->tag);
  }
}

yur_Ref *yu__in_as_baBin_pl(yur_Ref *x2244, yur_Ref *x2243) {
  switch (x2244->tag) {
  case 0: {
    yur_unref(x2244);
    return x2243;
  }
  case 1: {
    yur_Ref *x2245;
    x2245 = (x2244)->fields[0];
    yur_inc(x2245);
    yur_Ref *x2246;
    x2246 = (x2244)->fields[1];
    yur_inc(x2246);
    switch (x2246->tag) {
    case 0: {
      yur_unref(x2246);
      x2244 = yur_reset(x2244, 2);
      yur_Ref *x2249;
      x2249 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_as_baBin_pl)(x2245, x2243);
      yur_Ref *x18026;
      x18026 = &yur_unit;
      yur_inc(x18026);
      yur_Ref *x18028;
      x18028 = x2244;
      x18028->tag = 1;
      x18028->fields[0] = x2249;
      x18028->fields[1] = x18026;
      return x18028;
    }
    case 1: {
      yur_unref(x2246);
      x2244 = yur_reset(x2244, 2);
      yur_inc(x2243);
      yur_Ref *x2258;
      x2258 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_as_baBin_pl)(x2245, x2243);
      yur_Ref *x18025;
      x18025 = &yur_unit;
      yur_inc(x18025);
      yur_Ref *x18027;
      x18027 = x2244;
      x18027->tag = 1;
      x18027->fields[0] = x2258;
      x18027->fields[1] = x18025;
      yur_Ref *x14386;
      x14386 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_pl_baBin_pl)(x2243, x18027);
      return x14386;
    }
    default:
      yur_panic("unhandled case %zu", x2246->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x2244->tag);
  }
}

yur_Ref *yu__pr_mi_mi_baBin_pl(yur_Ref *x2266) {
  switch (x2266->tag) {
  case 0: {
    yur_unref(x2266);
    yur_Ref *x18034;
    x18034 = &yur_unit;
    yur_inc(x18034);
    return x18034;
  }
  case 1: {
    yur_Ref *x2267;
    x2267 = (x2266)->fields[0];
    yur_inc(x2267);
    yur_Ref *x2268;
    x2268 = (x2266)->fields[1];
    yur_inc(x2268);
    switch (x2268->tag) {
    case 0: {
      yur_unref(x2268);
      switch (x2267->tag) {
      case 0: {
        yur_unref(x2266);
        yur_unref(x2267);
        yur_Ref *x18033;
        x18033 = &yur_unit;
        yur_inc(x18033);
        return x18033;
      }
      case 1: {
        x2266 = yur_reset(x2266, 2);
        yur_Ref *x2271;
        x2271 = ((yur_Ref *(*)(yur_Ref *))yu__pr_mi_mi_baBin_pl)(x2267);
        yur_Ref *x18029;
        x18029 = &yu__ct1;
        yur_inc(x18029);
        yur_Ref *x18032;
        x18032 = x2266;
        x18032->tag = 1;
        x18032->fields[0] = x2271;
        x18032->fields[1] = x18029;
        return x18032;
      }
      default:
        yur_panic("unhandled case %zu", x2267->tag);
      }
    }
    case 1: {
      yur_unref(x2268);
      x2266 = yur_reset(x2266, 2);
      yur_Ref *x18030;
      x18030 = &yur_unit;
      yur_inc(x18030);
      yur_Ref *x18031;
      x18031 = x2266;
      x18031->tag = 1;
      x18031->fields[0] = x2267;
      x18031->fields[1] = x18030;
      return x18031;
    }
    default:
      yur_panic("unhandled case %zu", x2268->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x2266->tag);
  }
}

yur_Ref *yu__pocmp_baBin_pl(yur_Ref *x2278, yur_Ref *x2277) {
  switch (x2277->tag) {
  case 0: {
    yur_unref(x2277);
    switch (x2278->tag) {
    case 0: {
      yur_unref(x2278);
      yur_Ref *x18039;
      x18039 = &yu__ct1;
      yur_inc(x18039);
      return x18039;
    }
    case 1: {
      yur_unref(x2278);
      yur_Ref *x18043;
      x18043 = &yur_unit;
      yur_inc(x18043);
      return x18043;
    }
    default:
      yur_panic("unhandled case %zu", x2278->tag);
    }
  }
  case 1: {
    yur_Ref *x2281;
    x2281 = (x2277)->fields[0];
    yur_inc(x2281);
    yur_Ref *x2282;
    x2282 = (x2277)->fields[1];
    yur_inc(x2282);
    yur_unref(x2277);
    switch (x2278->tag) {
    case 0: {
      yur_unref(x2278);
      yur_unref(x2281);
      yur_unref(x2282);
      yur_Ref *x18038;
      x18038 = &yu__ct2;
      yur_inc(x18038);
      return x18038;
    }
    case 1: {
      yur_Ref *x2283;
      x2283 = (x2278)->fields[0];
      yur_inc(x2283);
      yur_Ref *x2284;
      x2284 = (x2278)->fields[1];
      yur_inc(x2284);
      yur_unref(x2278);
      switch (x2284->tag) {
      case 0: {
        yur_unref(x2284);
        switch (x2282->tag) {
        case 0: {
          yur_unref(x2282);
          yur_Ref *x14390;
          x14390 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__pocmp_baBin_pl)(x2283, x2281);
          return x14390;
        }
        case 1: {
          yur_unref(x2282);
          yur_Ref *x2288;
          x2288 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__pocmp_baBin_pl)(x2283, x2281);
          switch (x2288->tag) {
          case 0: {
            yur_unref(x2288);
            yur_Ref *x18042;
            x18042 = &yur_unit;
            yur_inc(x18042);
            return x18042;
          }
          case 1: {
            yur_unref(x2288);
            yur_Ref *x18037;
            x18037 = &yu__ct2;
            yur_inc(x18037);
            return x18037;
          }
          case 2: {
            yur_unref(x2288);
            yur_Ref *x18036;
            x18036 = &yu__ct2;
            yur_inc(x18036);
            return x18036;
          }
          default:
            yur_panic("unhandled case %zu", x2288->tag);
          }
        }
        default:
          yur_panic("unhandled case %zu", x2282->tag);
        }
      }
      case 1: {
        yur_unref(x2284);
        switch (x2282->tag) {
        case 0: {
          yur_unref(x2282);
          yur_Ref *x2292;
          x2292 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__pocmp_baBin_pl)(x2283, x2281);
          switch (x2292->tag) {
          case 0: {
            yur_unref(x2292);
            yur_Ref *x18041;
            x18041 = &yur_unit;
            yur_inc(x18041);
            return x18041;
          }
          case 1: {
            yur_unref(x2292);
            yur_Ref *x18040;
            x18040 = &yur_unit;
            yur_inc(x18040);
            return x18040;
          }
          case 2: {
            yur_unref(x2292);
            yur_Ref *x18035;
            x18035 = &yu__ct2;
            yur_inc(x18035);
            return x18035;
          }
          default:
            yur_panic("unhandled case %zu", x2292->tag);
          }
        }
        case 1: {
          yur_unref(x2282);
          yur_Ref *x14391;
          x14391 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__pocmp_baBin_pl)(x2283, x2281);
          return x14391;
        }
        default:
          yur_panic("unhandled case %zu", x2282->tag);
        }
      }
      default:
        yur_panic("unhandled case %zu", x2284->tag);
      }
    }
    default:
      yur_panic("unhandled case %zu", x2278->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x2277->tag);
  }
}

yur_Ref *yu_Nondet(yur_Ref *x2473, yur_Ref *x2472) {
  yur_unref(x2473);
  yur_Ref *x20607;
  x20607 = yur_build(1, 0);
  x20607->fields[0] = x2472;
  return x20607;
}

yur_Ref *yu_isFunctorEnv_doNondetC(yur_Ref *x2664, yur_Ref *x2663, yur_Ref *x2662, yur_Ref *x2661) {
  yur_unref(x2663);
  yur_unref(x2664);
  yur_Ref *x21986;
  x21986 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0_pomap_ba_poList_baTy)(x2662, x2661);
  return x21986;
}

yur_Ref *yu__in_co_gr_baTy(yur_Ref *x4000, yur_Ref *x3999) {
  switch (x3999->tag) {
  case 0: {
    yur_unref(x3999);
    return x4000;
  }
  case 1: {
    yur_unref(x3999);
    yur_unref(x4000);
    yur_Ref *x21987;
    x21987 = &yur_unit;
    yur_inc(x21987);
    return x21987;
  }
  default:
    yur_panic("unhandled case %zu", x3999->tag);
  }
}

yur_Ref *yu_isFunctor_doid(yur_Ref *x4439, yur_Ref *x4438, yur_Ref *x4437, yur_Ref *x4436) {
  yur_unref(x4438);
  yur_unref(x4439);
  yur_Ref *x20617;
  x20617 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x4437->tag)(x4436, x4437);
  return x20617;
}



yur_Ref *yu_aux0____poprint_baStr(yur_Ref *x4682) {
  switch (x4682->tag) {
  case 0: {
    yur_unref(x4682);
    yur_Ref *x18073;
    x18073 = &yur_unit;
    yur_inc(x18073);
    return x18073;
  }
  case 1: {
    yur_Ref *x4685;
    x4685 = (x4682)->fields[0];
    yur_inc(x4685);
    yur_Ref *x4686;
    x4686 = (x4682)->fields[1];
    yur_inc(x4686);
    yur_unref(x4682);
    yur_Ref *x4687;
    x4687 = ((yur_Ref *(*)(yur_Ref *))yu__poprint_baChar)(x4685);
    yur_unref(x4687);
    yur_Ref *x14619;
    x14619 = ((yur_Ref *(*)(yur_Ref *))yu_aux0____poprint_baStr)(x4686);
    return x14619;
  }
  default:
    yur_panic("unhandled case %zu", x4682->tag);
  }
}

yur_Ref *yu__postr_baNat(yur_Ref *x6279) {
  switch (x6279->tag) {
  case 0: {
    yur_unref(x6279);
    yur_Ref *x18090;
    x18090 = &yur_unit;
    yur_inc(x18090);
    yur_Ref *x18123;
    x18123 = &yur_unit;
    yur_inc(x18123);
    yur_Ref *x20627;
    x20627 = yur_build(2, 1);
    x20627->fields[0] = x18123;
    x20627->fields[1] = x18090;
    yur_Ref *x18155;
    x18155 = yur_build(1, 0);
    x18155->fields[0] = x20627;
    return x18155;
  }
  case 1: {
    yur_Ref *x6289;
    x6289 = (x6279)->fields[0];
    yur_inc(x6289);
    switch (x6289->tag) {
    case 0: {
      yur_unref(x6289);
      x6279 = yur_reset(x6279, 1);
      yur_Ref *x18089;
      x18089 = &yur_unit;
      yur_inc(x18089);
      yur_Ref *x18122;
      x18122 = &yu__ct1;
      yur_inc(x18122);
      yur_Ref *x20626;
      x20626 = yur_build(2, 1);
      x20626->fields[0] = x18122;
      x20626->fields[1] = x18089;
      yur_Ref *x18154;
      x18154 = x6279;
      x18154->tag = 0;
      x18154->fields[0] = x20626;
      return x18154;
    }
    case 1: {
      yur_Ref *x6299;
      x6299 = (x6289)->fields[0];
      yur_inc(x6299);
      switch (x6299->tag) {
      case 0: {
        yur_unref(x6279);
        yur_unref(x6299);
        x6289 = yur_reset(x6289, 1);
        yur_Ref *x18088;
        x18088 = &yur_unit;
        yur_inc(x18088);
        yur_Ref *x18121;
        x18121 = &yu__ct2;
        yur_inc(x18121);
        yur_Ref *x20625;
        x20625 = yur_build(2, 1);
        x20625->fields[0] = x18121;
        x20625->fields[1] = x18088;
        yur_Ref *x18153;
        x18153 = x6289;
        x18153->tag = 0;
        x18153->fields[0] = x20625;
        return x18153;
      }
      case 1: {
        yur_unref(x6289);
        yur_Ref *x6309;
        x6309 = (x6299)->fields[0];
        yur_inc(x6309);
        switch (x6309->tag) {
        case 0: {
          yur_unref(x6279);
          yur_unref(x6309);
          x6299 = yur_reset(x6299, 1);
          yur_Ref *x18087;
          x18087 = &yur_unit;
          yur_inc(x18087);
          yur_Ref *x18120;
          x18120 = &yu__ct3;
          yur_inc(x18120);
          yur_Ref *x20624;
          x20624 = yur_build(2, 1);
          x20624->fields[0] = x18120;
          x20624->fields[1] = x18087;
          yur_Ref *x18152;
          x18152 = x6299;
          x18152->tag = 0;
          x18152->fields[0] = x20624;
          return x18152;
        }
        case 1: {
          yur_unref(x6299);
          yur_Ref *x6319;
          x6319 = (x6309)->fields[0];
          yur_inc(x6319);
          switch (x6319->tag) {
          case 0: {
            yur_unref(x6279);
            yur_unref(x6319);
            x6309 = yur_reset(x6309, 1);
            yur_Ref *x18086;
            x18086 = &yur_unit;
            yur_inc(x18086);
            yur_Ref *x18119;
            x18119 = &yu__ct4;
            yur_inc(x18119);
            yur_Ref *x20623;
            x20623 = yur_build(2, 1);
            x20623->fields[0] = x18119;
            x20623->fields[1] = x18086;
            yur_Ref *x18151;
            x18151 = x6309;
            x18151->tag = 0;
            x18151->fields[0] = x20623;
            return x18151;
          }
          case 1: {
            yur_unref(x6309);
            yur_Ref *x6329;
            x6329 = (x6319)->fields[0];
            yur_inc(x6329);
            switch (x6329->tag) {
            case 0: {
              yur_unref(x6279);
              yur_unref(x6329);
              x6319 = yur_reset(x6319, 1);
              yur_Ref *x18085;
              x18085 = &yur_unit;
              yur_inc(x18085);
              yur_Ref *x18118;
              x18118 = &yu__ct5;
              yur_inc(x18118);
              yur_Ref *x20622;
              x20622 = yur_build(2, 1);
              x20622->fields[0] = x18118;
              x20622->fields[1] = x18085;
              yur_Ref *x18150;
              x18150 = x6319;
              x18150->tag = 0;
              x18150->fields[0] = x20622;
              return x18150;
            }
            case 1: {
              yur_unref(x6319);
              yur_Ref *x6339;
              x6339 = (x6329)->fields[0];
              yur_inc(x6339);
              switch (x6339->tag) {
              case 0: {
                yur_unref(x6279);
                yur_unref(x6339);
                x6329 = yur_reset(x6329, 1);
                yur_Ref *x18084;
                x18084 = &yur_unit;
                yur_inc(x18084);
                yur_Ref *x18117;
                x18117 = &yu__ct6;
                yur_inc(x18117);
                yur_Ref *x20621;
                x20621 = yur_build(2, 1);
                x20621->fields[0] = x18117;
                x20621->fields[1] = x18084;
                yur_Ref *x18149;
                x18149 = x6329;
                x18149->tag = 0;
                x18149->fields[0] = x20621;
                return x18149;
              }
              case 1: {
                yur_unref(x6329);
                yur_Ref *x6349;
                x6349 = (x6339)->fields[0];
                yur_inc(x6349);
                switch (x6349->tag) {
                case 0: {
                  yur_unref(x6279);
                  yur_unref(x6349);
                  x6339 = yur_reset(x6339, 1);
                  yur_Ref *x18083;
                  x18083 = &yur_unit;
                  yur_inc(x18083);
                  yur_Ref *x18116;
                  x18116 = &yu__ct7;
                  yur_inc(x18116);
                  yur_Ref *x20620;
                  x20620 = yur_build(2, 1);
                  x20620->fields[0] = x18116;
                  x20620->fields[1] = x18083;
                  yur_Ref *x18148;
                  x18148 = x6339;
                  x18148->tag = 0;
                  x18148->fields[0] = x20620;
                  return x18148;
                }
                case 1: {
                  yur_unref(x6339);
                  yur_Ref *x6359;
                  x6359 = (x6349)->fields[0];
                  yur_inc(x6359);
                  switch (x6359->tag) {
                  case 0: {
                    yur_unref(x6279);
                    yur_unref(x6359);
                    x6349 = yur_reset(x6349, 1);
                    yur_Ref *x18082;
                    x18082 = &yur_unit;
                    yur_inc(x18082);
                    yur_Ref *x18115;
                    x18115 = &yu__ct8;
                    yur_inc(x18115);
                    yur_Ref *x20619;
                    x20619 = yur_build(2, 1);
                    x20619->fields[0] = x18115;
                    x20619->fields[1] = x18082;
                    yur_Ref *x18147;
                    x18147 = x6349;
                    x18147->tag = 0;
                    x18147->fields[0] = x20619;
                    return x18147;
                  }
                  case 1: {
                    yur_unref(x6349);
                    yur_Ref *x6369;
                    x6369 = (x6359)->fields[0];
                    yur_inc(x6369);
                    switch (x6369->tag) {
                    case 0: {
                      yur_unref(x6279);
                      yur_unref(x6369);
                      x6359 = yur_reset(x6359, 1);
                      yur_Ref *x18081;
                      x18081 = &yur_unit;
                      yur_inc(x18081);
                      yur_Ref *x18114;
                      x18114 = &yu__ct9;
                      yur_inc(x18114);
                      yur_Ref *x20618;
                      x20618 = yur_build(2, 1);
                      x20618->fields[0] = x18114;
                      x20618->fields[1] = x18081;
                      yur_Ref *x18146;
                      x18146 = x6359;
                      x18146->tag = 0;
                      x18146->fields[0] = x20618;
                      return x18146;
                    }
                    case 1: {
                      yur_unref(x6359);
                      x6369 = yur_reset(x6369, 1);
                      yur_Ref *x16283;
                      x16283 = yur_ALOAD(yu__im10.fields[0]);
                      if (yur_LIKELY(yur_ALOAD(yu__im10.tag))) {
                        
                      } else {
                        yur_Ref *expect = x16283;
                        x16283 = yu_10();
                        yur_memoize(&yu__im10, &yu__im10.fields[0], &x16283, expect);
                        yur_ASTORE(yu__im10.tag, 1);
                      }
                      yur_inc(x16283);
                      yur_Ref *x18101;
                      x18101 = (x16283)->fields[0];
                      yur_inc(x18101);
                      yur_unref(x16283);
                      yur_Ref *x20628;
                      x20628 = &yur_unit;
                      yur_inc(x20628);
                      yur_inc(x18101);
                      yur_inc(x18101);
                      yur_Ref *x18105;
                      x18105 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu_aux0____podivmod_baNat)(x18101, x20628, x6279, x18101);
                      yur_Ref *x18107;
                      x18107 = (x18105)->fields[0];
                      yur_inc(x18107);
                      yur_Ref *x18108;
                      x18108 = (x18105)->fields[1];
                      yur_inc(x18108);
                      x18105 = yur_reset(x18105, 2);
                      yur_Ref *x18111;
                      x18111 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_mi_baNat)(x18108, x18101);
                      yur_Ref *x22433;
                      x22433 = x18105;
                      x22433->tag = 0;
                      x22433->fields[0] = x18107;
                      x22433->fields[1] = x18111;
                      yur_Ref *x6406;
                      x6406 = (x22433)->fields[0];
                      yur_inc(x6406);
                      yur_Ref *x6407;
                      x6407 = (x22433)->fields[1];
                      yur_inc(x6407);
                      yur_unref(x22433);
                      yur_Ref *x6413;
                      x6413 = ((yur_Ref *(*)(yur_Ref *))yu__postr_baNat)(x6406);
                      yur_Ref *x6410;
                      x6410 = ((yur_Ref *(*)(yur_Ref *))yu__postr_baNat)(x6407);
                      yur_Ref *x20631;
                      x20631 = (x6413)->fields[0];
                      yur_inc(x20631);
                      yur_unref(x6413);
                      yur_Ref *x21989;
                      x21989 = (x6410)->fields[0];
                      yur_inc(x21989);
                      yur_unref(x6410);
                      yur_Ref *x22434;
                      x22434 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pofold_ba_poList_baTy)(&yu__im_fn0_in_co_co_ba_poList_baTy, x21989, x20631);
                      yur_Ref *x20633;
                      x20633 = x6369;
                      x20633->tag = 0;
                      x20633->fields[0] = x22434;
                      return x20633;
                    }
                    default:
                      yur_panic("unhandled case %zu", x6369->tag);
                    }
                  }
                  default:
                    yur_panic("unhandled case %zu", x6359->tag);
                  }
                }
                default:
                  yur_panic("unhandled case %zu", x6349->tag);
                }
              }
              default:
                yur_panic("unhandled case %zu", x6339->tag);
              }
            }
            default:
              yur_panic("unhandled case %zu", x6329->tag);
            }
          }
          default:
            yur_panic("unhandled case %zu", x6319->tag);
          }
        }
        default:
          yur_panic("unhandled case %zu", x6309->tag);
        }
      }
      default:
        yur_panic("unhandled case %zu", x6299->tag);
      }
    }
    default:
      yur_panic("unhandled case %zu", x6289->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x6279->tag);
  }
}

yur_Ref *yu_aux0____podivmod_baNat(yur_Ref *x7570, yur_Ref *x7569, yur_Ref *x7568, yur_Ref *x7566) {
  switch (x7568->tag) {
  case 0: {
    yur_unref(x7566);
    yur_unref(x7568);
    yur_Ref *x20634;
    x20634 = yur_build(2, 0);
    x20634->fields[0] = x7569;
    x20634->fields[1] = x7570;
    return x20634;
  }
  case 1: {
    yur_Ref *x7578;
    x7578 = (x7568)->fields[0];
    yur_inc(x7578);
    switch (x7570->tag) {
    case 0: {
      yur_unref(x7570);
      x7568 = yur_reset(x7568, 1);
      yur_Ref *x18158;
      x18158 = x7568;
      x18158->tag = 1;
      x18158->fields[0] = x7569;
      yur_inc(x7566);
      yur_Ref *x15212;
      x15212 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu_aux0____podivmod_baNat)(x7566, x18158, x7578, x7566);
      return x15212;
    }
    case 1: {
      yur_unref(x7568);
      yur_Ref *x7585;
      x7585 = (x7570)->fields[0];
      yur_inc(x7585);
      yur_unref(x7570);
      yur_Ref *x15213;
      x15213 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu_aux0____podivmod_baNat)(x7585, x7569, x7578, x7566);
      return x15213;
    }
    default:
      yur_panic("unhandled case %zu", x7570->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x7568->tag);
  }
}

yur_Ref *yu__poeq_qu_baOrder(yur_Ref *x13611) {
  switch (x13611->tag) {
  case 0: {
    yur_unref(x13611);
    yur_Ref *x18162;
    x18162 = &yur_unit;
    yur_inc(x18162);
    return x18162;
  }
  case 1: {
    yur_unref(x13611);
    yur_Ref *x18160;
    x18160 = &yu__ct1;
    yur_inc(x18160);
    return x18160;
  }
  case 2: {
    yur_unref(x13611);
    yur_Ref *x18161;
    x18161 = &yur_unit;
    yur_inc(x18161);
    return x18161;
  }
  default:
    yur_panic("unhandled case %zu", x13611->tag);
  }
}

yur_Ref *yu_1() {
  yur_Ref *x18165;
  x18165 = &yur_unit;
  yur_inc(x18165);
  yur_Ref *x18164;
  x18164 = yur_build(1, 1);
  x18164->fields[0] = x18165;
  return x18164;
}

yur_Ref *yu_2() {
  yur_Ref *x16731;
  x16731 = yur_ALOAD(yu__im1.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im1.tag))) {
    
  } else {
    yur_Ref *expect = x16731;
    x16731 = yu_1();
    yur_memoize(&yu__im1, &yu__im1.fields[0], &x16731, expect);
    yur_ASTORE(yu__im1.tag, 1);
  }
  yur_inc(x16731);
  yur_Ref *x18166;
  x18166 = yur_build(1, 1);
  x18166->fields[0] = x16731;
  return x18166;
}

yur_Ref *yu_3() {
  yur_Ref *x16732;
  x16732 = yur_ALOAD(yu__im2.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im2.tag))) {
    
  } else {
    yur_Ref *expect = x16732;
    x16732 = yu_2();
    yur_memoize(&yu__im2, &yu__im2.fields[0], &x16732, expect);
    yur_ASTORE(yu__im2.tag, 1);
  }
  yur_inc(x16732);
  yur_Ref *x18167;
  x18167 = yur_build(1, 1);
  x18167->fields[0] = x16732;
  return x18167;
}

yur_Ref *yu_4() {
  yur_Ref *x16733;
  x16733 = yur_ALOAD(yu__im3.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im3.tag))) {
    
  } else {
    yur_Ref *expect = x16733;
    x16733 = yu_3();
    yur_memoize(&yu__im3, &yu__im3.fields[0], &x16733, expect);
    yur_ASTORE(yu__im3.tag, 1);
  }
  yur_inc(x16733);
  yur_Ref *x18168;
  x18168 = yur_build(1, 1);
  x18168->fields[0] = x16733;
  return x18168;
}

yur_Ref *yu_5() {
  yur_Ref *x16734;
  x16734 = yur_ALOAD(yu__im4.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im4.tag))) {
    
  } else {
    yur_Ref *expect = x16734;
    x16734 = yu_4();
    yur_memoize(&yu__im4, &yu__im4.fields[0], &x16734, expect);
    yur_ASTORE(yu__im4.tag, 1);
  }
  yur_inc(x16734);
  yur_Ref *x18169;
  x18169 = yur_build(1, 1);
  x18169->fields[0] = x16734;
  return x18169;
}

yur_Ref *yu_6() {
  yur_Ref *x16735;
  x16735 = yur_ALOAD(yu__im5.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im5.tag))) {
    
  } else {
    yur_Ref *expect = x16735;
    x16735 = yu_5();
    yur_memoize(&yu__im5, &yu__im5.fields[0], &x16735, expect);
    yur_ASTORE(yu__im5.tag, 1);
  }
  yur_inc(x16735);
  yur_Ref *x18170;
  x18170 = yur_build(1, 1);
  x18170->fields[0] = x16735;
  return x18170;
}

yur_Ref *yu_7() {
  yur_Ref *x16736;
  x16736 = yur_ALOAD(yu__im6.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im6.tag))) {
    
  } else {
    yur_Ref *expect = x16736;
    x16736 = yu_6();
    yur_memoize(&yu__im6, &yu__im6.fields[0], &x16736, expect);
    yur_ASTORE(yu__im6.tag, 1);
  }
  yur_inc(x16736);
  yur_Ref *x18171;
  x18171 = yur_build(1, 1);
  x18171->fields[0] = x16736;
  return x18171;
}

yur_Ref *yu_8() {
  yur_Ref *x16737;
  x16737 = yur_ALOAD(yu__im7.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im7.tag))) {
    
  } else {
    yur_Ref *expect = x16737;
    x16737 = yu_7();
    yur_memoize(&yu__im7, &yu__im7.fields[0], &x16737, expect);
    yur_ASTORE(yu__im7.tag, 1);
  }
  yur_inc(x16737);
  yur_Ref *x18172;
  x18172 = yur_build(1, 1);
  x18172->fields[0] = x16737;
  return x18172;
}

yur_Ref *yu_9() {
  yur_Ref *x16738;
  x16738 = yur_ALOAD(yu__im8.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im8.tag))) {
    
  } else {
    yur_Ref *expect = x16738;
    x16738 = yu_8();
    yur_memoize(&yu__im8, &yu__im8.fields[0], &x16738, expect);
    yur_ASTORE(yu__im8.tag, 1);
  }
  yur_inc(x16738);
  yur_Ref *x18173;
  x18173 = yur_build(1, 1);
  x18173->fields[0] = x16738;
  return x18173;
}

yur_Ref *yu_10() {
  yur_Ref *x16739;
  x16739 = yur_ALOAD(yu__im9.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im9.tag))) {
    
  } else {
    yur_Ref *expect = x16739;
    x16739 = yu_9();
    yur_memoize(&yu__im9, &yu__im9.fields[0], &x16739, expect);
    yur_ASTORE(yu__im9.tag, 1);
  }
  yur_inc(x16739);
  yur_Ref *x18174;
  x18174 = yur_build(1, 1);
  x18174->fields[0] = x16739;
  return x18174;
}

yur_Ref *yu__in_mi_baNat(yur_Ref *x13901, yur_Ref *x13900) {
  switch (x13900->tag) {
  case 0: {
    yur_unref(x13900);
    yur_unref(x13901);
    yur_Ref *x18175;
    x18175 = &yur_unit;
    yur_inc(x18175);
    return x18175;
  }
  case 1: {
    yur_Ref *x13902;
    x13902 = (x13900)->fields[0];
    yur_inc(x13902);
    switch (x13901->tag) {
    case 0: {
      yur_unref(x13901);
      yur_unref(x13902);
      return x13900;
    }
    case 1: {
      yur_unref(x13900);
      yur_Ref *x13903;
      x13903 = (x13901)->fields[0];
      yur_inc(x13903);
      yur_unref(x13901);
      yur_Ref *x15631;
      x15631 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__in_mi_baNat)(x13903, x13902);
      return x15631;
    }
    default:
      yur_panic("unhandled case %zu", x13901->tag);
    }
  }
  default:
    yur_panic("unhandled case %zu", x13900->tag);
  }
}

yur_Ref *yu__fn0makeTestList(yur_Ref *x1) {
  return x1;
}

yur_Ref *yu__fn1makeTestList(yur_Ref *x1, yur_Ref *x0) {
  yur_inc(x0);
  yur_Ref *x15;
  x15 = ((yur_Ref *(*)(yur_Ref *))yu__pr_mi_mi_baBin)(x0);
  yur_Ref *x20639;
  x20639 = yur_build(2, 1);
  x20639->fields[0] = x0;
  x20639->fields[1] = x1;
  yur_Ref *x20648;
  x20648 = &yur_unit;
  yur_inc(x20648);
  yur_inc(x15);
  yur_Ref *x20646;
  x20646 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__pocmp_baBin)(x20648, x15);
  yur_Ref *x20647;
  x20647 = ((yur_Ref *(*)(yur_Ref *))yu__poeq_qu_baOrder)(x20646);
  yur_Ref *x22436;
  x22436 = yur_build(1, 0);
  x22436->fields[0] = x20647;
  yur_Ref *x18189;
  x18189 = yur_alloc(2);
  yur_init(x18189, 1, 0);
  x18189->fields[0] = (yur_Ref *) &yu__im_pa_fn0makeTestList1;
  yur_inc(x20639);
  x18189->nfields = 2;
  x18189->fields[1] = x20639;
  yur_Ref *x22435;
  x22435 = yur_build(2, 0);
  x22435->fields[0] = x22436;
  x22435->fields[1] = x18189;
  yur_Ref *x18192;
  x18192 = yur_alloc(3);
  yur_init(x18192, 1, 0);
  x18192->fields[0] = (yur_Ref *) &yu__im_pa_fn1makeTestList2;
  x18192->nfields = 3;
  x18192->fields[1] = x15;
  x18192->fields[2] = x20639;
  yur_Ref *x21996;
  x21996 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0_poelse_baThen_doBool)(x18192, x22435);
  return x21996;
}

yur_Ref *yu__fn13timeTestChoice1(yur_Ref *x293, yur_Ref *x152, yur_Ref *x151, yur_Ref *x105, yur_Ref *x67) {
  yur_inc(x152);
  yur_Ref *x16828;
  x16828 = yur_build(5, (size_t) &yu__pa_fn17timeTestChoice15);
  x16828->fields[0] = x67;
  x16828->fields[1] = x105;
  x16828->fields[2] = x151;
  x16828->fields[3] = x152;
  x16828->fields[4] = x293;
  yur_Ref *x20811;
  x20811 = &yur_unit;
  yur_inc(x20811);
  yur_Ref *x22045;
  x22045 = &yur_unit;
  yur_inc(x22045);
  yur_Ref *x22467;
  x22467 = yur_build(2, 1);
  x22467->fields[0] = &yu__im_fn0State;
  x22467->fields[1] = x22045;
  yur_Ref *x22468;
  x22468 = yur_build(2, 1);
  x22468->fields[0] = &yu__imNondet;
  x22468->fields[1] = x22467;
  yur_Ref *x22046;
  x22046 = &yur_unit;
  yur_inc(x22046);
  yur_Ref *x22469;
  x22469 = yur_build(2, 1);
  x22469->fields[0] = &yu__im_fn0State;
  x22469->fields[1] = x22046;
  yur_Ref *x22047;
  x22047 = yur_build(1, 1);
  x22047->fields[0] = x22469;
  yur_Ref *x22470;
  x22470 = yur_build(2, 0);
  x22470->fields[0] = x22468;
  x22470->fields[1] = x22047;
  yur_Ref *x18448;
  x18448 = yur_ALOAD(yu__imtestList.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__imtestList.tag))) {
    
  } else {
    yur_Ref *expect = x18448;
    x18448 = yu_testList();
    yur_memoize(&yu__imtestList, &yu__imtestList.fields[0], &x18448, expect);
    yur_ASTORE(yu__imtestList.tag, 1);
  }
  yur_inc(x18448);
  yur_inc(x152);
  yur_Ref *x22471;
  x22471 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pochoose_baIn)(x18448, x152, x22470, x20811);
  yur_Ref *x18450;
  x18450 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x16828, x22471, x152);
  return x18450;
}

yur_Ref *yu__fn17timeTestChoice1(yur_Ref *x434, yur_Ref *x293, yur_Ref *x152, yur_Ref *x151, yur_Ref *x105, yur_Ref *x67) {
  yur_inc(x152);
  yur_Ref *x16864;
  x16864 = yur_build(6, (size_t) &yu__pa_fn21timeTestChoice16);
  x16864->fields[0] = x67;
  x16864->fields[1] = x105;
  x16864->fields[2] = x151;
  x16864->fields[3] = x152;
  x16864->fields[4] = x293;
  x16864->fields[5] = x434;
  yur_Ref *x20877;
  x20877 = &yur_unit;
  yur_inc(x20877);
  yur_Ref *x22053;
  x22053 = &yur_unit;
  yur_inc(x22053);
  yur_Ref *x22478;
  x22478 = yur_build(2, 1);
  x22478->fields[0] = &yu__im_fn0State;
  x22478->fields[1] = x22053;
  yur_Ref *x22479;
  x22479 = yur_build(2, 1);
  x22479->fields[0] = &yu__imNondet;
  x22479->fields[1] = x22478;
  yur_Ref *x22054;
  x22054 = &yur_unit;
  yur_inc(x22054);
  yur_Ref *x22480;
  x22480 = yur_build(2, 1);
  x22480->fields[0] = &yu__im_fn0State;
  x22480->fields[1] = x22054;
  yur_Ref *x22055;
  x22055 = yur_build(1, 1);
  x22055->fields[0] = x22480;
  yur_Ref *x22481;
  x22481 = yur_build(2, 0);
  x22481->fields[0] = x22479;
  x22481->fields[1] = x22055;
  yur_Ref *x18575;
  x18575 = yur_ALOAD(yu__imtestList.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__imtestList.tag))) {
    
  } else {
    yur_Ref *expect = x18575;
    x18575 = yu_testList();
    yur_memoize(&yu__imtestList, &yu__imtestList.fields[0], &x18575, expect);
    yur_ASTORE(yu__imtestList.tag, 1);
  }
  yur_inc(x18575);
  yur_inc(x152);
  yur_Ref *x22482;
  x22482 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pochoose_baIn)(x18575, x152, x22481, x20877);
  yur_Ref *x18577;
  x18577 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x16864, x22482, x152);
  return x18577;
}

yur_Ref *yu__fn21timeTestChoice1(yur_Ref *x575, yur_Ref *x434, yur_Ref *x293, yur_Ref *x152, yur_Ref *x151, yur_Ref *x105, yur_Ref *x67) {
  yur_inc(x152);
  yur_Ref *x16900;
  x16900 = yur_build(7, (size_t) &yu__pa_fn25timeTestChoice17);
  x16900->fields[0] = x67;
  x16900->fields[1] = x105;
  x16900->fields[2] = x151;
  x16900->fields[3] = x152;
  x16900->fields[4] = x293;
  x16900->fields[5] = x434;
  x16900->fields[6] = x575;
  yur_Ref *x22061;
  x22061 = &yur_unit;
  yur_inc(x22061);
  yur_Ref *x22483;
  x22483 = yur_build(2, 1);
  x22483->fields[0] = &yu__im_fn0State;
  x22483->fields[1] = x22061;
  yur_Ref *x22484;
  x22484 = yur_build(2, 1);
  x22484->fields[0] = &yu__imNondet;
  x22484->fields[1] = x22483;
  yur_Ref *x22062;
  x22062 = &yur_unit;
  yur_inc(x22062);
  yur_Ref *x22485;
  x22485 = yur_build(2, 1);
  x22485->fields[0] = &yu__im_fn0State;
  x22485->fields[1] = x22062;
  yur_Ref *x22063;
  x22063 = &yur_unit;
  yur_inc(x22063);
  yur_Ref *x22064;
  x22064 = yur_build(1, 1);
  x22064->fields[0] = x22063;
  yur_Ref *x22486;
  x22486 = yur_build(2, 0);
  x22486->fields[0] = x22485;
  x22486->fields[1] = x22064;
  yur_Ref *x22487;
  x22487 = yur_build(2, 0);
  x22487->fields[0] = x22484;
  x22487->fields[1] = x22486;
  yur_Ref *x18712;
  x18712 = yur_ALOAD(yu__im1_doBin.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im1_doBin.tag))) {
    
  } else {
    yur_Ref *expect = x18712;
    x18712 = yu_1_doBin();
    yur_memoize(&yu__im1_doBin, &yu__im1_doBin.fields[0], &x18712, expect);
    yur_ASTORE(yu__im1_doBin.tag, 1);
  }
  yur_inc(x18712);
  yur_Ref *x22068;
  x22068 = &yur_unit;
  yur_inc(x22068);
  yur_Ref *x22488;
  x22488 = yur_build(1, 1);
  x22488->fields[0] = x18712;
  yur_inc(x152);
  yur_Ref *x22071;
  x22071 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__fn0performShift)(x152, x22487, x22488, x22068, &yu__im_fn0State);
  yur_Ref *x18714;
  x18714 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x16900, x22071, x152);
  return x18714;
}

yur_Ref *yu__fn25timeTestChoice1(yur_Ref *x730, yur_Ref *x575, yur_Ref *x434, yur_Ref *x293, yur_Ref *x152, yur_Ref *x151, yur_Ref *x105, yur_Ref *x67) {
  yur_unref(x67);
  yur_unref(x105);
  yur_unref(x151);
  yur_unref(x730);
  yur_Ref *x22073;
  x22073 = yur_build(2, 0);
  x22073->fields[0] = x434;
  x22073->fields[1] = x575;
  yur_Ref *x22074;
  x22074 = yur_build(2, 0);
  x22074->fields[0] = x293;
  x22074->fields[1] = x22073;
  yur_Ref *x22076;
  x22076 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22074, x152);
  return x22076;
}

yur_Ref *yu__fn13timeTestChoice2(yur_Ref *x1057, yur_Ref *x916, yur_Ref *x915, yur_Ref *x869, yur_Ref *x831) {
  yur_unref(x1057);
  yur_inc(x916);
  yur_Ref *x21064;
  x21064 = yur_build(4, (size_t) &yu__pa_fn17timeTestChoice24);
  x21064->fields[0] = x831;
  x21064->fields[1] = x869;
  x21064->fields[2] = x915;
  x21064->fields[3] = x916;
  yur_Ref *x22161;
  x22161 = &yur_unit;
  yur_inc(x22161);
  yur_Ref *x22128;
  x22128 = &yur_unit;
  yur_inc(x22128);
  yur_Ref *x22519;
  x22519 = yur_build(2, 1);
  x22519->fields[0] = &yu__im_fn0State;
  x22519->fields[1] = x22128;
  yur_Ref *x22520;
  x22520 = yur_build(2, 1);
  x22520->fields[0] = &yu__imNondet;
  x22520->fields[1] = x22519;
  yur_Ref *x22129;
  x22129 = &yur_unit;
  yur_inc(x22129);
  yur_Ref *x22521;
  x22521 = yur_build(2, 1);
  x22521->fields[0] = &yu__im_fn0State;
  x22521->fields[1] = x22129;
  yur_Ref *x22130;
  x22130 = yur_build(1, 1);
  x22130->fields[0] = x22521;
  yur_Ref *x22523;
  x22523 = yur_build(2, 0);
  x22523->fields[0] = x22520;
  x22523->fields[1] = x22130;
  yur_Ref *x21146;
  x21146 = yur_ALOAD(yu__imtestList.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__imtestList.tag))) {
    
  } else {
    yur_Ref *expect = x21146;
    x21146 = yu_testList();
    yur_memoize(&yu__imtestList, &yu__imtestList.fields[0], &x21146, expect);
    yur_ASTORE(yu__imtestList.tag, 1);
  }
  yur_inc(x21146);
  yur_inc(x916);
  yur_Ref *x22525;
  x22525 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pochoose_baIn)(x21146, x916, x22523, x22161);
  yur_Ref *x22524;
  x22524 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21064, x22525, x916);
  return x22524;
}

yur_Ref *yu__fn17timeTestChoice2(yur_Ref *x1198, yur_Ref *x916, yur_Ref *x915, yur_Ref *x869, yur_Ref *x831) {
  yur_unref(x1198);
  yur_inc(x916);
  yur_Ref *x21151;
  x21151 = yur_build(4, (size_t) &yu__pa_fn21timeTestChoice24);
  x21151->fields[0] = x831;
  x21151->fields[1] = x869;
  x21151->fields[2] = x915;
  x21151->fields[3] = x916;
  yur_Ref *x22203;
  x22203 = &yur_unit;
  yur_inc(x22203);
  yur_Ref *x22170;
  x22170 = &yur_unit;
  yur_inc(x22170);
  yur_Ref *x22532;
  x22532 = yur_build(2, 1);
  x22532->fields[0] = &yu__im_fn0State;
  x22532->fields[1] = x22170;
  yur_Ref *x22533;
  x22533 = yur_build(2, 1);
  x22533->fields[0] = &yu__imNondet;
  x22533->fields[1] = x22532;
  yur_Ref *x22171;
  x22171 = &yur_unit;
  yur_inc(x22171);
  yur_Ref *x22534;
  x22534 = yur_build(2, 1);
  x22534->fields[0] = &yu__im_fn0State;
  x22534->fields[1] = x22171;
  yur_Ref *x22172;
  x22172 = yur_build(1, 1);
  x22172->fields[0] = x22534;
  yur_Ref *x22536;
  x22536 = yur_build(2, 0);
  x22536->fields[0] = x22533;
  x22536->fields[1] = x22172;
  yur_Ref *x21233;
  x21233 = yur_ALOAD(yu__imtestList.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__imtestList.tag))) {
    
  } else {
    yur_Ref *expect = x21233;
    x21233 = yu_testList();
    yur_memoize(&yu__imtestList, &yu__imtestList.fields[0], &x21233, expect);
    yur_ASTORE(yu__imtestList.tag, 1);
  }
  yur_inc(x21233);
  yur_inc(x916);
  yur_Ref *x22538;
  x22538 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pochoose_baIn)(x21233, x916, x22536, x22203);
  yur_Ref *x22537;
  x22537 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21151, x22538, x916);
  return x22537;
}

yur_Ref *yu__fn21timeTestChoice2(yur_Ref *x1339, yur_Ref *x916, yur_Ref *x915, yur_Ref *x869, yur_Ref *x831) {
  yur_unref(x1339);
  yur_inc(x916);
  yur_Ref *x21238;
  x21238 = yur_build(4, (size_t) &yu__pa_fn25timeTestChoice24);
  x21238->fields[0] = x831;
  x21238->fields[1] = x869;
  x21238->fields[2] = x915;
  x21238->fields[3] = x916;
  yur_Ref *x22212;
  x22212 = &yur_unit;
  yur_inc(x22212);
  yur_Ref *x22539;
  x22539 = yur_build(2, 1);
  x22539->fields[0] = &yu__im_fn0State;
  x22539->fields[1] = x22212;
  yur_Ref *x22540;
  x22540 = yur_build(2, 1);
  x22540->fields[0] = &yu__imNondet;
  x22540->fields[1] = x22539;
  yur_Ref *x22213;
  x22213 = &yur_unit;
  yur_inc(x22213);
  yur_Ref *x22541;
  x22541 = yur_build(2, 1);
  x22541->fields[0] = &yu__im_fn0State;
  x22541->fields[1] = x22213;
  yur_Ref *x22214;
  x22214 = &yur_unit;
  yur_inc(x22214);
  yur_Ref *x22215;
  x22215 = yur_build(1, 1);
  x22215->fields[0] = x22214;
  yur_Ref *x22543;
  x22543 = yur_build(2, 0);
  x22543->fields[0] = x22541;
  x22543->fields[1] = x22215;
  yur_Ref *x22544;
  x22544 = yur_build(2, 0);
  x22544->fields[0] = x22540;
  x22544->fields[1] = x22543;
  yur_Ref *x21330;
  x21330 = yur_ALOAD(yu__im1_doBin.fields[0]);
  if (yur_LIKELY(yur_ALOAD(yu__im1_doBin.tag))) {
    
  } else {
    yur_Ref *expect = x21330;
    x21330 = yu_1_doBin();
    yur_memoize(&yu__im1_doBin, &yu__im1_doBin.fields[0], &x21330, expect);
    yur_ASTORE(yu__im1_doBin.tag, 1);
  }
  yur_inc(x21330);
  yur_Ref *x22753;
  x22753 = &yur_unit;
  yur_inc(x22753);
  yur_Ref *x22754;
  x22754 = yur_build(1, 1);
  x22754->fields[0] = x21330;
  yur_inc(x916);
  yur_Ref *x22755;
  x22755 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__fn0performShift)(x916, x22544, x22754, x22753, &yu__im_fn0State);
  yur_Ref *x22545;
  x22545 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21238, x22755, x916);
  return x22545;
}

yur_Ref *yu__fn25timeTestChoice2(yur_Ref *x1494, yur_Ref *x916, yur_Ref *x915, yur_Ref *x869, yur_Ref *x831) {
  yur_unref(x831);
  yur_unref(x869);
  yur_unref(x915);
  yur_unref(x1494);
  yur_Ref *x22265;
  x22265 = &yur_unit;
  yur_inc(x22265);
  yur_Ref *x21345;
  x21345 = &yur_unit;
  yur_inc(x21345);
  yur_Ref *x22262;
  x22262 = yur_build(2, 1);
  x22262->fields[0] = &yu__im_fn0State;
  x22262->fields[1] = x21345;
  yur_Ref *x22263;
  x22263 = yur_build(2, 1);
  x22263->fields[0] = &yu__imNondet;
  x22263->fields[1] = x22262;
  yur_Ref *x21349;
  x21349 = &yur_unit;
  yur_inc(x21349);
  yur_Ref *x22264;
  x22264 = yur_build(2, 1);
  x22264->fields[0] = &yu__im_fn0State;
  x22264->fields[1] = x21349;
  yur_Ref *x21351;
  x21351 = yur_build(1, 1);
  x21351->fields[0] = x22264;
  yur_Ref *x22266;
  x22266 = yur_build(2, 0);
  x22266->fields[0] = x22263;
  x22266->fields[1] = x21351;
  yur_Ref *x22268;
  x22268 = &yur_unit;
  yur_inc(x22268);
  yur_Ref *x22269;
  x22269 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__fn0performShift)(x916, x22266, x22268, x22265, &yu__imNondet);
  return x22269;
}

yur_Ref *yu__fn2_poalt_baIn(yur_Ref *x2529, yur_Ref *x2510, yur_Ref *x2509) {
  switch (x2529->tag) {
  case 0: {
    yur_unref(x2509);
    yur_unref(x2529);
    return x2510;
  }
  case 1: {
    yur_unref(x2510);
    yur_unref(x2529);
    return x2509;
  }
  default:
    yur_panic("unhandled case %zu", x2529->tag);
  }
}

yur_Ref *yu__fn0chooseAll_doNondetC(yur_Ref *x2582, yur_Ref *x2581, yur_Ref *x2580, yur_Ref *x2579, yur_Ref *x2578, yur_Ref *x2577, yur_Ref *x2576) {
  switch (x2582->tag) {
  case 0: {
    yur_unref(x2576);
    yur_unref(x2577);
    yur_unref(x2578);
    yur_unref(x2579);
    yur_unref(x2581);
    yur_unref(x2582);
    yur_Ref *x19432;
    x19432 = &yur_unit;
    yur_inc(x19432);
    yur_Ref *x21374;
    x21374 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x19432, x2580);
    return x21374;
  }
  case 1: {
    yur_Ref *x2596;
    x2596 = (x2582)->fields[0];
    yur_inc(x2596);
    yur_Ref *x2597;
    x2597 = (x2582)->fields[1];
    yur_inc(x2597);
    yur_unref(x2582);
    yur_inc(x2580);
    yur_inc(x2581);
    yur_Ref *x17225;
    x17225 = yur_build(7, (size_t) &yu__pa_fn2chooseAll_doNondetC7);
    x17225->fields[0] = x2576;
    x17225->fields[1] = x2577;
    x17225->fields[2] = x2578;
    x17225->fields[3] = x2579;
    x17225->fields[4] = x2580;
    x17225->fields[5] = x2581;
    x17225->fields[6] = x2597;
    yur_Ref *x19430;
    x19430 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x2581->tag)(x2596, x2581);
    yur_Ref *x19431;
    x19431 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x17225, x19430, x2580);
    return x19431;
  }
  default:
    yur_panic("unhandled case %zu", x2582->tag);
  }
}

yur_Ref *yu__fn2chooseAll_doNondetC(yur_Ref *x2618, yur_Ref *x2597, yur_Ref *x2581, yur_Ref *x2580, yur_Ref *x2579, yur_Ref *x2578, yur_Ref *x2577, yur_Ref *x2576) {
  yur_inc(x2580);
  yur_Ref *x17227;
  x17227 = yur_build(2, (size_t) &yu__pa_du_du_fn4chooseAll_doNondetC2);
  x17227->fields[0] = x2580;
  x17227->fields[1] = x2618;
  yur_inc(x2580);
  yur_Ref *x19442;
  x19442 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__fn0chooseAll_doNondetC)(x2597, x2581, x2580, x2579, x2578, x2577, x2576);
  yur_Ref *x19443;
  x19443 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x17227, x19442, x2580);
  return x19443;
}

yur_Ref *yu__fn0unit_doNondetC(yur_Ref *x2675, yur_Ref *x2674, yur_Ref *x2673, yur_Ref *x2672, yur_Ref *x2671) {
  yur_unref(x2671);
  yur_unref(x2672);
  yur_unref(x2673);
  yur_Ref *x21389;
  x21389 = &yur_unit;
  yur_inc(x21389);
  yur_Ref *x22282;
  x22282 = yur_build(2, 1);
  x22282->fields[0] = x2675;
  x22282->fields[1] = x21389;
  yur_Ref *x22283;
  x22283 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22282, x2674);
  return x22283;
}

yur_Ref *yu__fn0modify_doNondetC(yur_Ref *x2698, yur_Ref *x2697, yur_Ref *x2696, yur_Ref *x2695, yur_Ref *x2694, yur_Ref *x2693, yur_Ref *x2692) {
  yur_Ref *x19454;
  x19454 = &yur_unit;
  yur_inc(x19454);
  yur_Ref *x19459;
  x19459 = &yur_unit;
  yur_inc(x19459);
  yur_Ref *x21392;
  x21392 = yur_build(2, 1);
  x21392->fields[0] = x19459;
  x21392->fields[1] = x19454;
  yur_Ref *x2721;
  x2721 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *)) x2697->tag)(&yu__im_fn1modify_doNondetC, x21392, x2697);
  yur_inc(x2696);
  yur_Ref *x17232;
  x17232 = yur_build(6, (size_t) &yu__pa_fn0chooseAll_doNondetC6);
  x17232->fields[0] = x2693;
  x17232->fields[1] = x2694;
  x17232->fields[2] = x2692;
  x17232->fields[3] = x2695;
  x17232->fields[4] = x2696;
  x17232->fields[5] = x2698;
  yur_Ref *x21393;
  x21393 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x17232, x2721, x2696);
  return x21393;
}

yur_Ref *yu__fn1modify_doNondetC(yur_Ref *x2727, yur_Ref *x2726, yur_Ref *x2725, yur_Ref *x2724) {
  yur_unref(x2724);
  yur_unref(x2725);
  yur_unref(x2726);
  return x2727;
}

yur_Ref *yu__fn0shift_doNondetC(yur_Ref *x2756, yur_Ref *x2755, yur_Ref *x2754, yur_Ref *x2753, yur_Ref *x2752, yur_Ref *x2751, yur_Ref *x2750) {
  yur_unref(x2750);
  yur_unref(x2751);
  yur_unref(x2752);
  yur_unref(x2753);
  yur_Ref *x22871;
  x22871 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_du_du_fn0shift_doNondetC)(x2756, x2755, x2754);
  return x22871;
}

yur_Ref *yu__fn0reset_doNondetC(yur_Ref *x2836, yur_Ref *x2835, yur_Ref *x2834, yur_Ref *x2833, yur_Ref *x2832, yur_Ref *x2831, yur_Ref *x2830, yur_Ref *x2829, yur_Ref *x2828, yur_Ref *x2827, yur_Ref *x2826) {
  yur_unref(x2826);
  yur_unref(x2827);
  yur_unref(x2828);
  yur_unref(x2829);
  yur_unref(x2830);
  yur_unref(x2831);
  yur_unref(x2832);
  yur_unref(x2834);
  yur_unref(x2835);
  yur_unref(x2836);
  yur_Ref *x20195;
  x20195 = ((yur_Ref *(*)(yur_Ref *))yu__du_fn0reset_doNondetC)(x2833);
  return x20195;
}

yur_Ref *yu__fn0State(yur_Ref *x2874, yur_Ref *x2873) {
  yur_unref(x2874);
  yur_Ref *x21415;
  x21415 = yur_build(1, 0);
  x21415->fields[0] = x2873;
  return x21415;
}

yur_Ref *yu__fn0isFunctorEnv_doStateC(yur_Ref *x2929, yur_Ref *x2928, yur_Ref *x2927, yur_Ref *x2926, yur_Ref *x2925) {
  yur_unref(x2925);
  yur_unref(x2928);
  yur_unref(x2929);
  yur_Ref *x22300;
  x22300 = (x2926)->fields[0];
  yur_inc(x22300);
  yur_Ref *x22299;
  x22299 = (x2926)->fields[1];
  yur_inc(x22299);
  x2926 = yur_reset(x2926, 2);
  yur_Ref *x21423;
  x21423 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x2927->tag)(x22299, x2927);
  yur_Ref *x22301;
  x22301 = x2926;
  x22301->tag = 0;
  x22301->fields[0] = x22300;
  x22301->fields[1] = x21423;
  return x22301;
}

yur_Ref *yu__fn0unit_doStateC(yur_Ref *x2956, yur_Ref *x2955, yur_Ref *x2954, yur_Ref *x2953, yur_Ref *x2952, yur_Ref *x2951) {
  yur_unref(x2951);
  yur_unref(x2952);
  yur_unref(x2954);
  yur_inc(x2955);
  yur_Ref *x22302;
  x22302 = yur_build(3, (size_t) &yu__pa_du_fn1unit_doStateC3);
  x22302->fields[0] = x2953;
  x22302->fields[1] = x2955;
  x22302->fields[2] = x2956;
  yur_Ref *x22303;
  x22303 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22302, x2955);
  return x22303;
}

yur_Ref *yu__fn0modify_doStateC(yur_Ref *x2996, yur_Ref *x2995, yur_Ref *x2994, yur_Ref *x2993, yur_Ref *x2992, yur_Ref *x2991, yur_Ref *x2990, yur_Ref *x2989) {
  yur_unref(x2991);
  yur_unref(x2992);
  yur_unref(x2993);
  yur_inc(x2994);
  yur_Ref *x22756;
  x22756 = yur_build(5, (size_t) &yu__pa_du_fn2modify_doStateC5);
  x22756->fields[0] = x2989;
  x22756->fields[1] = x2990;
  x22756->fields[2] = x2994;
  x22756->fields[3] = x2995;
  x22756->fields[4] = x2996;
  yur_Ref *x22757;
  x22757 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22756, x2994);
  return x22757;
}

yur_Ref *yu__fn0runC0___modify_doStateC(yur_Ref *x3001, yur_Ref *x3000, yur_Ref *x2999, yur_Ref *x2998, yur_Ref *x2997, yur_Ref *x2990, yur_Ref *x2989) {
  yur_unref(x2989);
  yur_unref(x2990);
  yur_unref(x2998);
  yur_unref(x2999);
  yur_Ref *x21438;
  x21438 = yur_build(1, (size_t) &yu__pa_fn2runC0___modify_doStateC1);
  x21438->fields[0] = x2997;
  yur_Ref *x22306;
  x22306 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21438, x3001, x3000);
  return x22306;
}

yur_Ref *yu__fn2runC0___modify_doStateC(yur_Ref *x3014, yur_Ref *x2997) {
  yur_Ref *x14444;
  x14444 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x3014->tag)(x2997, x3014);
  return x14444;
}

yur_Ref *yu__fn0shift_doStateC(yur_Ref *x3134, yur_Ref *x3133, yur_Ref *x3132, yur_Ref *x3131, yur_Ref *x3130, yur_Ref *x3129, yur_Ref *x3128, yur_Ref *x3127) {
  yur_unref(x3129);
  yur_Ref *x20325;
  x20325 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0shift_doStateC)(x3134, x3133, x3132, x3131, x3130, x3128, x3127);
  return x20325;
}

yur_Ref *yu__fn4shift_doStateC(yur_Ref *x3169, yur_Ref *x3151) {
  yur_Ref *x14454;
  x14454 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x3169->tag)(x3151, x3169);
  return x14454;
}

yur_Ref *yu__fn6shift_doStateC(yur_Ref *x3193, yur_Ref *x3177, yur_Ref *x3134, yur_Ref *x3132, yur_Ref *x3131, yur_Ref *x3130, yur_Ref *x3128, yur_Ref *x3127) {
  yur_unref(x3127);
  yur_unref(x3128);
  yur_unref(x3130);
  yur_unref(x3131);
  yur_unref(x3193);
  yur_Ref *x21468;
  x21468 = &yur_unit;
  yur_inc(x21468);
  yur_Ref *x21469;
  x21469 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x3134->tag)(x21468, x3134);
  yur_Ref *x21470;
  x21470 = yur_build(1, (size_t) &yu__pa_fn8shift_doStateC1);
  x21470->fields[0] = x3177;
  yur_Ref *x22311;
  x22311 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21470, x21469, x3132);
  return x22311;
}

yur_Ref *yu__fn8shift_doStateC(yur_Ref *x3211, yur_Ref *x3177) {
  yur_Ref *x14458;
  x14458 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x3211->tag)(x3177, x3211);
  return x14458;
}

yur_Ref *yu__fn0reset_doStateC(yur_Ref *x3230, yur_Ref *x3229, yur_Ref *x3228, yur_Ref *x3227, yur_Ref *x3226, yur_Ref *x3225, yur_Ref *x3224, yur_Ref *x3223, yur_Ref *x3222, yur_Ref *x3221, yur_Ref *x3220) {
  yur_unref(x3220);
  yur_unref(x3221);
  yur_unref(x3222);
  yur_unref(x3223);
  yur_unref(x3224);
  yur_unref(x3225);
  yur_unref(x3226);
  yur_unref(x3228);
  yur_unref(x3229);
  yur_unref(x3230);
  yur_Ref *x20374;
  x20374 = ((yur_Ref *(*)(yur_Ref *))yu__du_fn0reset_doStateC)(x3227);
  return x20374;
}

yur_Ref *yu__fn1run_doStateC(yur_Ref *x3276, yur_Ref *x3260) {
  yur_Ref *x14462;
  x14462 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x3276->tag)(x3260, x3276);
  return x14462;
}

yur_Ref *yu__fn0performShift(yur_Ref *x3452, yur_Ref *x3451, yur_Ref *x3450, yur_Ref *x3449, yur_Ref *x3446) {
  switch (x3451->tag) {
  case 0: {
    yur_Ref *x3456;
    x3456 = (x3451)->fields[1];
    yur_inc(x3456);
    yur_unref(x3451);
    yur_Ref *x3468;
    x3468 = (x3452)->fields[4];
    yur_inc(x3468);
    yur_unref(x3452);
    yur_Ref *x17279;
    x17279 = yur_build(5, (size_t) &yu__pa_du_fn5performShift5);
    x17279->fields[0] = x3446;
    x17279->fields[1] = x3449;
    x17279->fields[2] = x3450;
    x17279->fields[3] = x3456;
    x17279->fields[4] = x3468;
    return x17279;
  }
  case 1: {
    yur_unref(x3446);
    yur_unref(x3451);
    yur_Ref *x3501;
    x3501 = &yur_unit;
    yur_inc(x3501);
    yur_Ref *x3504;
    x3504 = (x3452)->fields[2];
    yur_inc(x3504);
    yur_Ref *x3505;
    x3505 = (x3452)->fields[3];
    yur_inc(x3505);
    yur_Ref *x3506;
    x3506 = (x3452)->fields[4];
    yur_inc(x3506);
    yur_unref(x3452);
    yur_Ref *x17280;
    x17280 = yur_build(6, (size_t) &yu__pa_du_fn9performShift6);
    x17280->fields[0] = x3449;
    x17280->fields[1] = x3450;
    x17280->fields[2] = x3501;
    x17280->fields[3] = x3504;
    x17280->fields[4] = x3505;
    x17280->fields[5] = x3506;
    return x17280;
  }
  case 2: {
    yur_unref(x3446);
    yur_Ref *x3528;
    x3528 = (x3451)->fields[1];
    yur_inc(x3528);
    yur_unref(x3451);
    yur_Ref *x3535;
    x3535 = &yur_unit;
    yur_inc(x3535);
    yur_Ref *x3538;
    x3538 = (x3452)->fields[2];
    yur_inc(x3538);
    yur_Ref *x3539;
    x3539 = (x3452)->fields[3];
    yur_inc(x3539);
    yur_Ref *x3540;
    x3540 = (x3452)->fields[4];
    yur_inc(x3540);
    yur_unref(x3452);
    yur_Ref *x17281;
    x17281 = yur_build(7, (size_t) &yu__pa_du_fn14performShift7);
    x17281->fields[0] = x3449;
    x17281->fields[1] = x3450;
    x17281->fields[2] = x3528;
    x17281->fields[3] = x3535;
    x17281->fields[4] = x3538;
    x17281->fields[5] = x3539;
    x17281->fields[6] = x3540;
    return x17281;
  }
  default:
    yur_panic("unhandled case %zu", x3451->tag);
  }
}

yur_Ref *yu__fn0isFunctor_doComp(yur_Ref *x4212) {
  switch (x4212->tag) {
  case 0: {
    yur_Ref *x4218;
    x4218 = (x4212)->fields[2];
    yur_inc(x4218);
    yur_unref(x4212);
    yur_Ref *x19597;
    x19597 = &yur_unit;
    yur_inc(x19597);
    yur_Ref *x17305;
    x17305 = yur_build(1, (size_t) &yu__pa_in_co_gr_baTy1);
    x17305->fields[0] = x4218;
    yur_Ref *x19598;
    x19598 = yur_build(2, (size_t) &yu__pa_fn0isFunctor_doContT2);
    x19598->fields[0] = x19597;
    x19598->fields[1] = x17305;
    return x19598;
  }
  case 1: {
    yur_unref(x4212);
    return &yu__imisFunctor_doid;
  }
  default:
    yur_panic("unhandled case %zu", x4212->tag);
  }
}

yur_Ref *yu__fn1map_doContT(yur_Ref *x4523, yur_Ref *x4522, yur_Ref *x4521, yur_Ref *x4520, yur_Ref *x4519, yur_Ref *x4518, yur_Ref *x4517) {
  yur_unref(x4517);
  yur_unref(x4518);
  yur_unref(x4519);
  yur_unref(x4522);
  yur_Ref *x22321;
  x22321 = yur_build(2, (size_t) &yu__pa_fn1_in_at_ba_in_mi_gr2);
  x22321->fields[0] = x4523;
  x22321->fields[1] = x4521;
  yur_Ref *x21550;
  x21550 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x4520->tag)(x22321, x4520);
  return x21550;
}

yur_Ref *yu__fn0isFunctor_doContT(yur_Ref *x4541, yur_Ref *x4540, yur_Ref *x4539, yur_Ref *x4538, yur_Ref *x4537, yur_Ref *x4536) {
  yur_Ref *x17316;
  x17316 = yur_build(1, (size_t) &yu__pa_fn1isFunctor_doContT1);
  x17316->fields[0] = x4537;
  yur_Ref *x21551;
  x21551 = yur_build(6, (size_t) &yu__pa_fn1map_doContT6);
  x21551->fields[0] = x4540;
  x21551->fields[1] = x4541;
  x21551->fields[2] = x17316;
  x21551->fields[3] = x4538;
  x21551->fields[4] = x4539;
  x21551->fields[5] = x4536;
  return x21551;
}

yur_Ref *yu__fn1isFunctor_doContT(yur_Ref *x4545, yur_Ref *x4537) {
  yur_Ref *x14594;
  x14594 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x4537->tag)(x4545, x4537);
  return x14594;
}

yur_Ref *yu__fn1pure_doContT(yur_Ref *x4557, yur_Ref *x4556) {
  yur_Ref *x14596;
  x14596 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x4557->tag)(x4556, x4557);
  return x14596;
}

yur_Ref *yu__fn1bind_doContT(yur_Ref *x4566, yur_Ref *x4565, yur_Ref *x4564) {
  yur_Ref *x17319;
  x17319 = yur_build(2, (size_t) &yu__pa_fn2bind_doContT2);
  x17319->fields[0] = x4565;
  x17319->fields[1] = x4566;
  yur_Ref *x14598;
  x14598 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x4564->tag)(x17319, x4564);
  return x14598;
}

yur_Ref *yu__fn2bind_doContT(yur_Ref *x4569, yur_Ref *x4566, yur_Ref *x4565) {
  yur_Ref *x4570;
  x4570 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x4565->tag)(x4569, x4565);
  yur_Ref *x14597;
  x14597 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x4570->tag)(x4566, x4570);
  return x14597;
}

yur_Ref *yu__fn0_po2_ba_pr_am_ba_in_mi_gr(yur_Ref *x13685) {
  yur_Ref *x13689;
  x13689 = (x13685)->fields[1];
  yur_inc(x13689);
  yur_unref(x13685);
  return x13689;
}

yur_Ref *yu__fn0_in_co_co_ba_poList_baTy(yur_Ref *x13938, yur_Ref *x13937) {
  yur_Ref *x15637;
  x15637 = yur_build(2, 1);
  x15637->fields[0] = x13937;
  x15637->fields[1] = x13938;
  return x15637;
}

yur_Ref *yu__fn1_poelse_baThen_doBool(yur_Ref *x14011, yur_Ref *x14004, yur_Ref *x14001) {
  yur_Ref *x14016;
  x14016 = yur_ALOAD(x14011->fields[0]);if (!yur_ALOAD(x14011->tag)) {
    yur_Ref *expect = x14016;
    x14016 = ((yur_Ref *(*)(yur_Ref *)) x14016->tag)(x14011);
    yur_memoize(x14011, &x14011->fields[0], &x14016, expect);
    yur_ASTORE(x14011->tag, 1);
  }
  yur_inc(x14016);
  switch (x14016->tag) {
  case 0: {
    yur_unref(x14004);
    yur_unref(x14016);
    yur_Ref *x15653;
    x15653 = yur_ALOAD(x14001->fields[0]);if (!yur_ALOAD(x14001->tag)) {
      yur_Ref *expect = x15653;
      x15653 = ((yur_Ref *(*)(yur_Ref *)) x15653->tag)(x14001);
      yur_memoize(x14001, &x14001->fields[0], &x15653, expect);
      yur_ASTORE(x14001->tag, 1);
    }
    yur_inc(x15653);
    return x15653;
  }
  case 1: {
    yur_unref(x14001);
    yur_unref(x14016);
    yur_Ref *x15654;
    x15654 = yur_ALOAD(x14004->fields[0]);if (!yur_ALOAD(x14004->tag)) {
      yur_Ref *expect = x15654;
      x15654 = ((yur_Ref *(*)(yur_Ref *)) x15654->tag)(x14004);
      yur_memoize(x14004, &x14004->fields[0], &x15654, expect);
      yur_ASTORE(x14004->tag, 1);
    }
    yur_inc(x15654);
    return x15654;
  }
  default:
    yur_panic("unhandled case %zu", x14016->tag);
  }
}

yur_Ref *yu__fn1_in_at_ba_in_mi_gr(yur_Ref *x14220, yur_Ref *x14219, yur_Ref *x14218) {
  yur_Ref *x14222;
  x14222 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x14219->tag)(x14220, x14219);
  yur_Ref *x15675;
  x15675 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x14218->tag)(x14222, x14218);
  return x15675;
}

yur_Ref *yu__du_fn0reset_doNondetC(yur_Ref *x20194) {
  switch (x20194->tag) {
  default:
    yur_panic("unhandled case %zu", x20194->tag);
  }
}

yur_Ref *yu__du_fn1unit_doStateC(yur_Ref *x20225, yur_Ref *x20224, yur_Ref *x20223, yur_Ref *x20222) {
  yur_unref(x20222);
  yur_Ref *x22328;
  x22328 = yur_build(2, 0);
  x22328->fields[0] = x20225;
  x22328->fields[1] = x20224;
  yur_Ref *x22329;
  x22329 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22328, x20223);
  return x22329;
}

yur_Ref *yu__du_fn9modify_doStateC(yur_Ref *x20301, yur_Ref *x20300) {
  yur_Ref *x21821;
  x21821 = (x20300)->fields[0];
  yur_inc(x21821);
  yur_unref(x20300);
  yur_Ref *x20303;
  x20303 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x20301->tag)(x21821, x20301);
  return x20303;
}

yur_Ref *yu__du_fn0shift_doStateC(yur_Ref *x20313, yur_Ref *x20312, yur_Ref *x20311, yur_Ref *x20310, yur_Ref *x20309, yur_Ref *x20308, yur_Ref *x20307) {
  switch (x20312->tag) {
  case 0: {
    yur_unref(x20307);
    yur_unref(x20308);
    yur_unref(x20309);
    yur_unref(x20310);
    yur_unref(x20312);
    yur_inc(x20311);
    yur_Ref *x20314;
    x20314 = yur_build(2, (size_t) &yu__pa_du_du_fn2shift_doStateC2);
    x20314->fields[0] = x20311;
    x20314->fields[1] = x20313;
    yur_Ref *x20318;
    x20318 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x20314, x20311);
    return x20318;
  }
  case 1: {
    yur_Ref *x20319;
    x20319 = (x20312)->fields[0];
    yur_inc(x20319);
    yur_unref(x20312);
    yur_inc(x20311);
    yur_Ref *x20320;
    x20320 = yur_build(7, (size_t) &yu__pa_fn6shift_doStateC7);
    x20320->fields[0] = x20307;
    x20320->fields[1] = x20308;
    x20320->fields[2] = x20309;
    x20320->fields[3] = x20310;
    x20320->fields[4] = x20311;
    x20320->fields[5] = x20313;
    x20320->fields[6] = x20319;
    yur_Ref *x20324;
    x20324 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x20320, x20311);
    return x20324;
  }
  default:
    yur_panic("unhandled case %zu", x20312->tag);
  }
}

yur_Ref *yu__du_fn0reset_doStateC(yur_Ref *x20373) {
  switch (x20373->tag) {
  default:
    yur_panic("unhandled case %zu", x20373->tag);
  }
}

yur_Ref *yu__du_fn1handle_doHandler(yur_Ref *x20409, yur_Ref *x20408, yur_Ref *x20407, yur_Ref *x20406, yur_Ref *x20405) {
  yur_Ref *x21835;
  x21835 = (x20407)->fields[2];
  yur_inc(x21835);
  yur_unref(x20407);
  yur_Ref *x21836;
  x21836 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *)) x21835->tag)(x20409, x20408, x20406, x20405, x21835);
  return x21836;
}

yur_Ref *yu__du_fn5performShift(yur_Ref *x20420, yur_Ref *x20419, yur_Ref *x20418, yur_Ref *x20417, yur_Ref *x20416, yur_Ref *x20415) {
  yur_inc(x20419);
  yur_Ref *x20424;
  x20424 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__fn0performShift)(x20419, x20418, x20417, x20416, x20415);
  yur_Ref *x21837;
  x21837 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x20420, x20424, x20419);
  return x21837;
}

yur_Ref *yu__du_fn9performShift(yur_Ref *x20435, yur_Ref *x20434, yur_Ref *x20433, yur_Ref *x20432, yur_Ref *x20431, yur_Ref *x20430, yur_Ref *x20429) {
  yur_Ref *x21838;
  x21838 = (x20433)->fields[2];
  yur_inc(x21838);
  yur_unref(x20433);
  yur_Ref *x21839;
  x21839 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *)) x21838->tag)(x20435, x20430, x20434, x20432, x20431, x20429, x21838);
  return x21839;
}

yur_Ref *yu__du_fn14performShift(yur_Ref *x20450, yur_Ref *x20449, yur_Ref *x20448, yur_Ref *x20447, yur_Ref *x20446, yur_Ref *x20445, yur_Ref *x20444, yur_Ref *x20443) {
  yur_Ref *x20452;
  x20452 = &yur_unit;
  yur_inc(x20452);
  yur_inc(x20443);
  yur_Ref *x20453;
  x20453 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *)) x20445->tag)(x20444, x20443, x20452, x20445);
  yur_Ref *x21840;
  x21840 = (x20448)->fields[2];
  yur_inc(x21840);
  yur_unref(x20448);
  yur_Ref *x21841;
  x21841 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *)) x21840->tag)(x20450, x20453, x20449, x20447, x20446, x20443, x21840);
  return x21841;
}

yur_Ref *yu__du_fn0pure_doComp(yur_Ref *x20467, yur_Ref *x20466) {
  switch (x20466->tag) {
  case 0: {
    yur_unref(x20466);
    yur_Ref *x22342;
    x22342 = yur_build(1, (size_t) &yu__pa_fn1pure_doContT1);
    x22342->fields[0] = x20467;
    return x22342;
  }
  case 1: {
    yur_unref(x20466);
    return x20467;
  }
  default:
    yur_panic("unhandled case %zu", x20466->tag);
  }
}

yur_Ref *yu__du_fn0bind_doComp(yur_Ref *x20477, yur_Ref *x20476, yur_Ref *x20475) {
  switch (x20475->tag) {
  case 0: {
    yur_unref(x20475);
    yur_Ref *x22343;
    x22343 = yur_build(2, (size_t) &yu__pa_fn1bind_doContT2);
    x22343->fields[0] = x20476;
    x22343->fields[1] = x20477;
    return x22343;
  }
  case 1: {
    yur_unref(x20475);
    yur_Ref *x20482;
    x20482 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x20477->tag)(x20476, x20477);
    return x20482;
  }
  default:
    yur_panic("unhandled case %zu", x20475->tag);
  }
}

yur_Ref *yu__du_fn0_polen_ba_poList_baTy(yur_Ref *x20505) {
  switch (x20505->tag) {
  case 0: {
    yur_unref(x20505);
    yur_Ref *x20506;
    x20506 = &yur_unit;
    yur_inc(x20506);
    return x20506;
  }
  case 1: {
    yur_Ref *x20507;
    x20507 = (x20505)->fields[1];
    yur_inc(x20507);
    yur_unref(x20505);
    yur_Ref *x21845;
    x21845 = ((yur_Ref *(*)(yur_Ref *))yu__du_fn0_polen_ba_poList_baTy)(x20507);
    yur_Ref *x20510;
    x20510 = yur_build(1, 1);
    x20510->fields[0] = x21845;
    return x20510;
  }
  default:
    yur_panic("unhandled case %zu", x20505->tag);
  }
}

yur_Ref *yu__du_fn0_pomap_ba_poList_baTy(yur_Ref *x20513, yur_Ref *x20512) {
  switch (x20512->tag) {
  case 0: {
    yur_unref(x20512);
    yur_unref(x20513);
    yur_Ref *x20514;
    x20514 = &yur_unit;
    yur_inc(x20514);
    return x20514;
  }
  case 1: {
    yur_Ref *x20515;
    x20515 = (x20512)->fields[0];
    yur_inc(x20515);
    yur_Ref *x20516;
    x20516 = (x20512)->fields[1];
    yur_inc(x20516);
    x20512 = yur_reset(x20512, 2);
    yur_inc(x20513);
    yur_Ref *x20517;
    x20517 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x20513->tag)(x20515, x20513);
    yur_Ref *x21847;
    x21847 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0_pomap_ba_poList_baTy)(x20513, x20516);
    yur_Ref *x21846;
    x21846 = x20512;
    x21846->tag = 1;
    x21846->fields[0] = x20517;
    x21846->fields[1] = x21847;
    return x21846;
  }
  default:
    yur_panic("unhandled case %zu", x20512->tag);
  }
}

yur_Ref *yu__du_fn0_pofold_ba_poList_baTy(yur_Ref *x20524, yur_Ref *x20523, yur_Ref *x20522) {
  switch (x20522->tag) {
  case 0: {
    yur_unref(x20522);
    yur_unref(x20524);
    return x20523;
  }
  case 1: {
    yur_Ref *x20525;
    x20525 = (x20522)->fields[0];
    yur_inc(x20525);
    yur_Ref *x20526;
    x20526 = (x20522)->fields[1];
    yur_inc(x20526);
    yur_unref(x20522);
    yur_inc(x20524);
    yur_Ref *x21848;
    x21848 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pofold_ba_poList_baTy)(x20524, x20523, x20526);
    yur_Ref *x20529;
    x20529 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *)) x20524->tag)(x21848, x20525, x20524);
    return x20529;
  }
  default:
    yur_panic("unhandled case %zu", x20522->tag);
  }
}

yur_Ref *yu__du_fn0_poelse_baThen_doBool(yur_Ref *x20538, yur_Ref *x20537) {
  yur_Ref *x20539;
  x20539 = (x20537)->fields[0];
  yur_inc(x20539);
  yur_Ref *x20540;
  x20540 = (x20537)->fields[1];
  yur_inc(x20540);
  yur_unref(x20537);
  switch (x20539->tag) {
  case 0: {
    yur_Ref *x20541;
    x20541 = (x20539)->fields[0];
    yur_inc(x20541);
    yur_unref(x20539);
    switch (x20541->tag) {
    case 0: {
      yur_unref(x20540);
      yur_unref(x20541);
      yur_Ref *x20542;
      x20542 = yur_ALOAD(x20538->fields[0]);if (!yur_ALOAD(x20538->tag)) {
        yur_Ref *expect = x20542;
        x20542 = ((yur_Ref *(*)(yur_Ref *)) x20542->tag)(x20538);
        yur_memoize(x20538, &x20538->fields[0], &x20542, expect);
        yur_ASTORE(x20538->tag, 1);
      }
      yur_inc(x20542);
      return x20542;
    }
    case 1: {
      yur_unref(x20538);
      yur_unref(x20541);
      yur_Ref *x20543;
      x20543 = yur_ALOAD(x20540->fields[0]);if (!yur_ALOAD(x20540->tag)) {
        yur_Ref *expect = x20543;
        x20543 = ((yur_Ref *(*)(yur_Ref *)) x20543->tag)(x20540);
        yur_memoize(x20540, &x20540->fields[0], &x20543, expect);
        yur_ASTORE(x20540->tag, 1);
      }
      yur_inc(x20543);
      return x20543;
    }
    default:
      yur_panic("unhandled case %zu", x20541->tag);
    }
  }
  case 1: {
    yur_Ref *x20544;
    x20544 = (x20539)->fields[0];
    yur_inc(x20544);
    yur_Ref *x20545;
    x20545 = (x20539)->fields[1];
    yur_inc(x20545);
    yur_unref(x20539);
    yur_Ref *x20547;
    x20547 = yur_alloc(4);
    yur_init(x20547, 1, 0);
    x20547->fields[0] = (yur_Ref *) &yu__im_pa_fn1_poelse_baThen_doBool3;
    x20547->nfields = 4;
    x20547->fields[1] = x20538;
    x20547->fields[2] = x20540;
    x20547->fields[3] = x20545;
    yur_Ref *x21849;
    x21849 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0_poelse_baThen_doBool)(x20547, x20544);
    return x21849;
  }
  default:
    yur_panic("unhandled case %zu", x20539->tag);
  }
}

yur_Ref *yu__du_du_fn6modify_doStateC(yur_Ref *x21913, yur_Ref *x21912, yur_Ref *x21911) {
  yur_Ref *x21914;
  x21914 = (x21913)->fields[1];
  yur_inc(x21914);
  yur_Ref *x21915;
  x21915 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x21912->tag)(x21914, x21912);
  yur_Ref *x21916;
  x21916 = yur_build(1, (size_t) &yu__pa_du_fn9modify_doStateC1);
  x21916->fields[0] = x21913;
  yur_Ref *x21917;
  x21917 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21916, x21915, x21911);
  return x21917;
}

yur_Ref *yu__du_du_fn2shift_doStateC(yur_Ref *x21921, yur_Ref *x21920, yur_Ref *x21919) {
  yur_inc(x21921);
  yur_Ref *x21922;
  x21922 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x21920->tag)(x21921, x21920);
  yur_Ref *x21923;
  x21923 = yur_build(1, (size_t) &yu__pa_fn4shift_doStateC1);
  x21923->fields[0] = x21921;
  yur_Ref *x21924;
  x21924 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x21923, x21922, x21919);
  return x21924;
}

yur_Ref *yu__du_fn0_pochoose_baIn(yur_Ref *x22350, yur_Ref *x22349, yur_Ref *x22348, yur_Ref *x22347) {
  switch (x22350->tag) {
  case 0: {
    yur_unref(x22350);
    yur_Ref *x22351;
    x22351 = &yur_unit;
    yur_inc(x22351);
    yur_Ref *x22352;
    x22352 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__fn0performShift)(x22349, x22348, x22351, x22347, &yu__imNondet);
    return x22352;
  }
  case 1: {
    yur_Ref *x22353;
    x22353 = (x22350)->fields[0];
    yur_inc(x22353);
    yur_Ref *x22354;
    x22354 = (x22350)->fields[1];
    yur_inc(x22354);
    yur_unref(x22350);
    yur_inc(x22349);
    yur_Ref *x22355;
    x22355 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22353, x22349);
    yur_inc(x22348);
    yur_inc(x22349);
    yur_Ref *x22563;
    x22563 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pochoose_baIn)(x22354, x22349, x22348, x22347);
    yur_Ref *x22357;
    x22357 = yur_build(2, (size_t) &yu__pa_fn2_poalt_baIn2);
    x22357->fields[0] = x22355;
    x22357->fields[1] = x22563;
    yur_Ref *x22762;
    x22762 = &yur_unit;
    yur_inc(x22762);
    yur_Ref *x22763;
    x22763 = &yu__ct1;
    yur_inc(x22763);
    yur_inc(x22349);
    yur_Ref *x22764;
    x22764 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *, yur_Ref *))yu__fn0performShift)(x22349, x22348, x22763, x22762, &yu__imNondet);
    yur_Ref *x22562;
    x22562 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x22357, x22764, x22349);
    return x22562;
  }
  default:
    yur_panic("unhandled case %zu", x22350->tag);
  }
}

yur_Ref *yu__du_fn2modify_doStateC(yur_Ref *x22369, yur_Ref *x22368, yur_Ref *x22367, yur_Ref *x22366, yur_Ref *x22365, yur_Ref *x22364) {
  yur_inc(x22366);
  yur_Ref *x22370;
  x22370 = yur_build(2, (size_t) &yu__pa_du_du_fn6modify_doStateC2);
  x22370->fields[0] = x22366;
  x22370->fields[1] = x22368;
  yur_Ref *x22372;
  x22372 = &yur_unit;
  yur_inc(x22372);
  yur_inc(x22369);
  yur_Ref *x22564;
  x22564 = yur_build(2, 0);
  x22564->fields[0] = x22369;
  x22564->fields[1] = x22372;
  yur_Ref *x22374;
  x22374 = yur_build(3, (size_t) &yu__pa_fn0runC0___modify_doStateC3);
  x22374->fields[0] = x22364;
  x22374->fields[1] = x22365;
  x22374->fields[2] = x22369;
  yur_Ref *x22375;
  x22375 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *)) x22367->tag)(x22374, x22564, x22367);
  yur_Ref *x22376;
  x22376 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x22370, x22375, x22366);
  return x22376;
}

yur_Ref *yu__du_du_fn4chooseAll_doNondetC(yur_Ref *x22380, yur_Ref *x22379, yur_Ref *x22378) {
  yur_Ref *x22381;
  x22381 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pofold_ba_poList_baTy)(&yu__im_fn0_in_co_co_ba_poList_baTy, x22380, x22379);
  yur_Ref *x22382;
  x22382 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22381, x22378);
  return x22382;
}

yur_Ref *yu__du_du_fn4shift_doNondetC(yur_Ref *x22398, yur_Ref *x22397, yur_Ref *x22396) {
  yur_Ref *x22399;
  x22399 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0_pofold_ba_poList_baTy)(&yu__im_fn0_in_co_co_ba_poList_baTy, x22398, x22397);
  yur_Ref *x22400;
  x22400 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22399, x22396);
  return x22400;
}

yur_Ref *yu__du_du_fn2shift_doNondetC(yur_Ref *x22577, yur_Ref *x22576, yur_Ref *x22575) {
  yur_inc(x22575);
  yur_Ref *x22578;
  x22578 = yur_build(2, (size_t) &yu__pa_du_du_fn4shift_doNondetC2);
  x22578->fields[0] = x22575;
  x22578->fields[1] = x22577;
  yur_Ref *x22579;
  x22579 = &yur_unit;
  yur_inc(x22579);
  yur_Ref *x22580;
  x22580 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x22576->tag)(x22579, x22576);
  yur_Ref *x22581;
  x22581 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x22578, x22580, x22575);
  return x22581;
}

yur_Ref *yu__du_du_du_fn0shift_doNondetC(yur_Ref *x22767, yur_Ref *x22766, yur_Ref *x22765) {
  switch (x22766->tag) {
  case 0: {
    yur_unref(x22766);
    yur_unref(x22767);
    yur_Ref *x22768;
    x22768 = &yur_unit;
    yur_inc(x22768);
    yur_Ref *x22769;
    x22769 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *))yu__du_fn0pure_doComp)(x22768, x22765);
    return x22769;
  }
  case 1: {
    yur_unref(x22766);
    yur_inc(x22765);
    yur_inc(x22767);
    yur_Ref *x22770;
    x22770 = yur_build(2, (size_t) &yu__pa_du_du_fn2shift_doNondetC2);
    x22770->fields[0] = x22765;
    x22770->fields[1] = x22767;
    yur_Ref *x22771;
    x22771 = &yu__ct1;
    yur_inc(x22771);
    yur_Ref *x22772;
    x22772 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *)) x22767->tag)(x22771, x22767);
    yur_Ref *x22773;
    x22773 = ((yur_Ref *(*)(yur_Ref *, yur_Ref *, yur_Ref *))yu__du_fn0bind_doComp)(x22770, x22772, x22765);
    return x22773;
  }
  default:
    yur_panic("unhandled case %zu", x22766->tag);
  }
}