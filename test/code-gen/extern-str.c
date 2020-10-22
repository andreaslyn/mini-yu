#include <yustr.h>

yur_Ref *yu_char_miid(yur_Ref *c) {
  return yur_cchar_to_yuchar(yur_yuchar_to_cchar(c));
}

yur_Ref *yu_str_miid(yur_Ref *s) {
  char *x = yur_yustr_to_cstr(s);
  yur_Ref *ret = yur_cstr_to_yustr(x);
  free(x);
  return ret;
}
