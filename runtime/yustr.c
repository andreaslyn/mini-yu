#include "yustr.h"
#include "yulist.h"
#include "yucmalloc.h"
#include <stdlib.h>
#include <stdio.h>

#define yur_CH_TAG_0 0
#define yur_CH_TAG_1 1
#define yur_CH_TAG_2 2
#define yur_CH_TAG_3 3
#define yur_CH_TAG_4 4
#define yur_CH_TAG_5 5
#define yur_CH_TAG_6 6
#define yur_CH_TAG_7 7
#define yur_CH_TAG_8 8
#define yur_CH_TAG_9 9
#define yur_CH_TAG_a 10
#define yur_CH_TAG_b 11
#define yur_CH_TAG_c 12
#define yur_CH_TAG_d 13
#define yur_CH_TAG_e 14
#define yur_CH_TAG_f 15
#define yur_CH_TAG_g 16
#define yur_CH_TAG_h 17
#define yur_CH_TAG_i 18
#define yur_CH_TAG_j 19
#define yur_CH_TAG_k 20
#define yur_CH_TAG_l 21
#define yur_CH_TAG_m 22
#define yur_CH_TAG_n 23
#define yur_CH_TAG_o 24
#define yur_CH_TAG_p 25
#define yur_CH_TAG_q 26
#define yur_CH_TAG_r 27
#define yur_CH_TAG_s 28
#define yur_CH_TAG_t 29
#define yur_CH_TAG_u 30
#define yur_CH_TAG_v 31
#define yur_CH_TAG_w 32
#define yur_CH_TAG_x 33
#define yur_CH_TAG_y 34
#define yur_CH_TAG_z 35
#define yur_CH_TAG_A 36
#define yur_CH_TAG_B 37
#define yur_CH_TAG_C 38
#define yur_CH_TAG_D 39
#define yur_CH_TAG_E 40
#define yur_CH_TAG_F 41
#define yur_CH_TAG_G 42
#define yur_CH_TAG_H 43
#define yur_CH_TAG_I 44
#define yur_CH_TAG_J 45
#define yur_CH_TAG_K 46
#define yur_CH_TAG_L 47
#define yur_CH_TAG_M 48
#define yur_CH_TAG_N 49
#define yur_CH_TAG_O 50
#define yur_CH_TAG_P 51
#define yur_CH_TAG_Q 52
#define yur_CH_TAG_R 53
#define yur_CH_TAG_S 54
#define yur_CH_TAG_T 55
#define yur_CH_TAG_U 56
#define yur_CH_TAG_V 57
#define yur_CH_TAG_W 58
#define yur_CH_TAG_X 59
#define yur_CH_TAG_Y 60
#define yur_CH_TAG_Z 61
#define yur_CH_TAG_null 62
#define yur_CH_TAG_line_feed 63
#define yur_CH_TAG_horizontal_tab 64
#define yur_CH_TAG_space 65
#define yur_CH_TAG_double_quote 66
#define yur_CH_TAG_pound 67
#define yur_CH_TAG_single_quote 68
#define yur_CH_TAG_caret 69
#define yur_CH_TAG_at 70
#define yur_CH_TAG_back_slash 71
#define yur_CH_TAG_forward_slash 72
#define yur_CH_TAG_asterisk 73
#define yur_CH_TAG_percent 74
#define yur_CH_TAG_dollar 75
#define yur_CH_TAG_vertical_bar 76
#define yur_CH_TAG_ampersand 77
#define yur_CH_TAG_plus 78
#define yur_CH_TAG_minus 79
#define yur_CH_TAG_question_mark 80
#define yur_CH_TAG_exclamation_mark 81
#define yur_CH_TAG_less_than 82
#define yur_CH_TAG_greater_than 83
#define yur_CH_TAG_equal 84
#define yur_CH_TAG_colon 85
#define yur_CH_TAG_tilde 86
#define yur_CH_TAG_start_of_heading 87
#define yur_CH_TAG_start_of_text 88
#define yur_CH_TAG_end_of_text 89
#define yur_CH_TAG_end_of_transmission 90
#define yur_CH_TAG_enquiry 91
#define yur_CH_TAG_acknowledge 92
#define yur_CH_TAG_bell 93
#define yur_CH_TAG_back_space 94
#define yur_CH_TAG_vertival_tab 95
#define yur_CH_TAG_form_feed 96
#define yur_CH_TAG_carriage_return 97
#define yur_CH_TAG_shift_out 98
#define yur_CH_TAG_shift_in 99
#define yur_CH_TAG_data_link_escape 100
#define yur_CH_TAG_device_control_1 101
#define yur_CH_TAG_device_control_2 102
#define yur_CH_TAG_device_control_3 103
#define yur_CH_TAG_device_control_4 104
#define yur_CH_TAG_negative_acknowledge 105
#define yur_CH_TAG_synchronous_idle 106
#define yur_CH_TAG_end_of_transmission_block 107
#define yur_CH_TAG_cancel 108
#define yur_CH_TAG_end_of_medium 109
#define yur_CH_TAG_substitute 110
#define yur_CH_TAG_escape 111
#define yur_CH_TAG_file_seperator 112
#define yur_CH_TAG_group_seperator 113
#define yur_CH_TAG_record_seperator 114
#define yur_CH_TAG_unit_seperator 115
#define yur_CH_TAG_left_parenthesis 116
#define yur_CH_TAG_right_parenthesis 117
#define yur_CH_TAG_left_square_bracket 118
#define yur_CH_TAG_right_square_bracket 119
#define yur_CH_TAG_left_curly_bracket 120
#define yur_CH_TAG_right_curly_bracket 121
#define yur_CH_TAG_period 122
#define yur_CH_TAG_comma 123
#define yur_CH_TAG_semicolon 124
#define yur_CH_TAG_underscore 125
#define yur_CH_TAG_grave_accent 126
#define yur_CH_TAG_delete 127

yur_Ref yur_ch_0 = {
  .count = 0,
  .tag = yur_CH_TAG_0,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_1 = {
  .count = 0,
  .tag = yur_CH_TAG_1,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_2 = {
  .count = 0,
  .tag = yur_CH_TAG_2,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_3 = {
  .count = 0,
  .tag = yur_CH_TAG_3,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_4 = {
  .count = 0,
  .tag = yur_CH_TAG_4,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_5 = {
  .count = 0,
  .tag = yur_CH_TAG_5,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_6 = {
  .count = 0,
  .tag = yur_CH_TAG_6,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_7 = {
  .count = 0,
  .tag = yur_CH_TAG_7,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_8 = {
  .count = 0,
  .tag = yur_CH_TAG_8,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_9 = {
  .count = 0,
  .tag = yur_CH_TAG_9,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_a = {
  .count = 0,
  .tag = yur_CH_TAG_a,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_b = {
  .count = 0,
  .tag = yur_CH_TAG_b,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_c = {
  .count = 0,
  .tag = yur_CH_TAG_c,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_d = {
  .count = 0,
  .tag = yur_CH_TAG_d,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_e = {
  .count = 0,
  .tag = yur_CH_TAG_e,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_f = {
  .count = 0,
  .tag = yur_CH_TAG_f,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_g = {
  .count = 0,
  .tag = yur_CH_TAG_g,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_h = {
  .count = 0,
  .tag = yur_CH_TAG_h,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_i = {
  .count = 0,
  .tag = yur_CH_TAG_i,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_j = {
  .count = 0,
  .tag = yur_CH_TAG_j,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_k = {
  .count = 0,
  .tag = yur_CH_TAG_k,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_l = {
  .count = 0,
  .tag = yur_CH_TAG_l,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_m = {
  .count = 0,
  .tag = yur_CH_TAG_m,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_n = {
  .count = 0,
  .tag = yur_CH_TAG_n,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_o = {
  .count = 0,
  .tag = yur_CH_TAG_o,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_p = {
  .count = 0,
  .tag = yur_CH_TAG_p,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_q = {
  .count = 0,
  .tag = yur_CH_TAG_q,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_r = {
  .count = 0,
  .tag = yur_CH_TAG_r,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_s = {
  .count = 0,
  .tag = yur_CH_TAG_s,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_t = {
  .count = 0,
  .tag = yur_CH_TAG_t,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_u = {
  .count = 0,
  .tag = yur_CH_TAG_u,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_v = {
  .count = 0,
  .tag = yur_CH_TAG_v,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_w = {
  .count = 0,
  .tag = yur_CH_TAG_w,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_x = {
  .count = 0,
  .tag = yur_CH_TAG_x,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_y = {
  .count = 0,
  .tag = yur_CH_TAG_y,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_z = {
  .count = 0,
  .tag = yur_CH_TAG_z,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_A = {
  .count = 0,
  .tag = yur_CH_TAG_A,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_B = {
  .count = 0,
  .tag = yur_CH_TAG_B,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_C = {
  .count = 0,
  .tag = yur_CH_TAG_C,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_D = {
  .count = 0,
  .tag = yur_CH_TAG_D,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_E = {
  .count = 0,
  .tag = yur_CH_TAG_E,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_F = {
  .count = 0,
  .tag = yur_CH_TAG_F,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_G = {
  .count = 0,
  .tag = yur_CH_TAG_G,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_H = {
  .count = 0,
  .tag = yur_CH_TAG_H,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_I = {
  .count = 0,
  .tag = yur_CH_TAG_I,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_J = {
  .count = 0,
  .tag = yur_CH_TAG_J,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_K = {
  .count = 0,
  .tag = yur_CH_TAG_K,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_L = {
  .count = 0,
  .tag = yur_CH_TAG_L,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_M = {
  .count = 0,
  .tag = yur_CH_TAG_M,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_N = {
  .count = 0,
  .tag = yur_CH_TAG_N,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_O = {
  .count = 0,
  .tag = yur_CH_TAG_O,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_P = {
  .count = 0,
  .tag = yur_CH_TAG_P,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_Q = {
  .count = 0,
  .tag = yur_CH_TAG_Q,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_R = {
  .count = 0,
  .tag = yur_CH_TAG_R,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_S = {
  .count = 0,
  .tag = yur_CH_TAG_S,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_T = {
  .count = 0,
  .tag = yur_CH_TAG_T,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_U = {
  .count = 0,
  .tag = yur_CH_TAG_U,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_V = {
  .count = 0,
  .tag = yur_CH_TAG_V,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_W = {
  .count = 0,
  .tag = yur_CH_TAG_W,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_X = {
  .count = 0,
  .tag = yur_CH_TAG_X,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_Y = {
  .count = 0,
  .tag = yur_CH_TAG_Y,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_Z = {
  .count = 0,
  .tag = yur_CH_TAG_Z,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_null = {
  .count = 0,
  .tag = yur_CH_TAG_null,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_line_feed = {
  .count = 0,
  .tag = yur_CH_TAG_line_feed,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_horizontal_tab = {
  .count = 0,
  .tag = yur_CH_TAG_horizontal_tab,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_space = {
  .count = 0,
  .tag = yur_CH_TAG_space,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_double_quote = {
  .count = 0,
  .tag = yur_CH_TAG_double_quote,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_pound = {
  .count = 0,
  .tag = yur_CH_TAG_pound,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_single_quote = {
  .count = 0,
  .tag = yur_CH_TAG_single_quote,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_caret = {
  .count = 0,
  .tag = yur_CH_TAG_caret,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_at = {
  .count = 0,
  .tag = yur_CH_TAG_at,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_back_slash = {
  .count = 0,
  .tag = yur_CH_TAG_back_slash,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_forward_slash = {
  .count = 0,
  .tag = yur_CH_TAG_forward_slash,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_asterisk = {
  .count = 0,
  .tag = yur_CH_TAG_asterisk,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_percent = {
  .count = 0,
  .tag = yur_CH_TAG_percent,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_dollar = {
  .count = 0,
  .tag = yur_CH_TAG_dollar,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_vertical_bar = {
  .count = 0,
  .tag = yur_CH_TAG_vertical_bar,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_ampersand = {
  .count = 0,
  .tag = yur_CH_TAG_ampersand,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_plus = {
  .count = 0,
  .tag = yur_CH_TAG_plus,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_minus = {
  .count = 0,
  .tag = yur_CH_TAG_minus,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_question_mark = {
  .count = 0,
  .tag = yur_CH_TAG_question_mark,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_exclamation_mark = {
  .count = 0,
  .tag = yur_CH_TAG_exclamation_mark,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_less_than = {
  .count = 0,
  .tag = yur_CH_TAG_less_than,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_greater_than = {
  .count = 0,
  .tag = yur_CH_TAG_greater_than,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_equal = {
  .count = 0,
  .tag = yur_CH_TAG_equal,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_colon = {
  .count = 0,
  .tag = yur_CH_TAG_colon,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_tilde = {
  .count = 0,
  .tag = yur_CH_TAG_tilde,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_start_of_heading = {
  .count = 0,
  .tag = yur_CH_TAG_start_of_heading,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_start_of_text = {
  .count = 0,
  .tag = yur_CH_TAG_start_of_text,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_end_of_text = {
  .count = 0,
  .tag = yur_CH_TAG_end_of_text,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_end_of_transmission = {
  .count = 0,
  .tag = yur_CH_TAG_end_of_transmission,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_enquiry = {
  .count = 0,
  .tag = yur_CH_TAG_enquiry,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_acknowledge = {
  .count = 0,
  .tag = yur_CH_TAG_acknowledge,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_bell = {
  .count = 0,
  .tag = yur_CH_TAG_bell,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_back_space = {
  .count = 0,
  .tag = yur_CH_TAG_back_space,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_vertival_tab = {
  .count = 0,
  .tag = yur_CH_TAG_vertival_tab,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_form_feed = {
  .count = 0,
  .tag = yur_CH_TAG_form_feed,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_carriage_return = {
  .count = 0,
  .tag = yur_CH_TAG_carriage_return,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_shift_out = {
  .count = 0,
  .tag = yur_CH_TAG_shift_out,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_shift_in = {
  .count = 0,
  .tag = yur_CH_TAG_shift_in,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_data_link_escape = {
  .count = 0,
  .tag = yur_CH_TAG_data_link_escape,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_device_control_1 = {
  .count = 0,
  .tag = yur_CH_TAG_device_control_1,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_device_control_2 = {
  .count = 0,
  .tag = yur_CH_TAG_device_control_2,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_device_control_3 = {
  .count = 0,
  .tag = yur_CH_TAG_device_control_3,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_device_control_4 = {
  .count = 0,
  .tag = yur_CH_TAG_device_control_4,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_negative_acknowledge = {
  .count = 0,
  .tag = yur_CH_TAG_negative_acknowledge,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_synchronous_idle = {
  .count = 0,
  .tag = yur_CH_TAG_synchronous_idle,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_end_of_transmission_block = {
  .count = 0,
  .tag = yur_CH_TAG_end_of_transmission_block,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_cancel = {
  .count = 0,
  .tag = yur_CH_TAG_cancel,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_end_of_medium = {
  .count = 0,
  .tag = yur_CH_TAG_end_of_medium,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_substitute = {
  .count = 0,
  .tag = yur_CH_TAG_substitute,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_escape = {
  .count = 0,
  .tag = yur_CH_TAG_escape,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_file_seperator = {
  .count = 0,
  .tag = yur_CH_TAG_file_seperator,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_group_seperator = {
  .count = 0,
  .tag = yur_CH_TAG_group_seperator,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_record_seperator = {
  .count = 0,
  .tag = yur_CH_TAG_record_seperator,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_unit_seperator = {
  .count = 0,
  .tag = yur_CH_TAG_unit_seperator,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_left_parenthesis = {
  .count = 0,
  .tag = yur_CH_TAG_left_parenthesis,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_right_parenthesis = {
  .count = 0,
  .tag = yur_CH_TAG_right_parenthesis,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_left_square_bracket = {
  .count = 0,
  .tag = yur_CH_TAG_left_square_bracket,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_right_square_bracket = {
  .count = 0,
  .tag = yur_CH_TAG_right_square_bracket,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_left_curly_bracket = {
  .count = 0,
  .tag = yur_CH_TAG_left_curly_bracket,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_right_curly_bracket = {
  .count = 0,
  .tag = yur_CH_TAG_right_curly_bracket,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_period = {
  .count = 0,
  .tag = yur_CH_TAG_period,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_comma = {
  .count = 0,
  .tag = yur_CH_TAG_comma,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_semicolon = {
  .count = 0,
  .tag = yur_CH_TAG_semicolon,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_underscore = {
  .count = 0,
  .tag = yur_CH_TAG_underscore,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_grave_accent = {
  .count = 0,
  .tag = yur_CH_TAG_grave_accent,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};
yur_Ref yur_ch_delete = {
  .count = 0,
  .tag = yur_CH_TAG_delete,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

char yur_yuchar_to_cchar(yur_Ref *c) {
  char ret;
  switch (c->tag) {
  case yur_CH_TAG_0: ret = '0'; break;
  case yur_CH_TAG_1: ret = '1'; break;
  case yur_CH_TAG_2: ret = '2'; break;
  case yur_CH_TAG_3: ret = '3'; break;
  case yur_CH_TAG_4: ret = '4'; break;
  case yur_CH_TAG_5: ret = '5'; break;
  case yur_CH_TAG_6: ret = '6'; break;
  case yur_CH_TAG_7: ret = '7'; break;
  case yur_CH_TAG_8: ret = '8'; break;
  case yur_CH_TAG_9: ret = '9'; break;
  case yur_CH_TAG_a: ret = 'a'; break;
  case yur_CH_TAG_b: ret = 'b'; break;
  case yur_CH_TAG_c: ret = 'c'; break;
  case yur_CH_TAG_d: ret = 'd'; break;
  case yur_CH_TAG_e: ret = 'e'; break;
  case yur_CH_TAG_f: ret = 'f'; break;
  case yur_CH_TAG_g: ret = 'g'; break;
  case yur_CH_TAG_h: ret = 'h'; break;
  case yur_CH_TAG_i: ret = 'i'; break;
  case yur_CH_TAG_j: ret = 'j'; break;
  case yur_CH_TAG_k: ret = 'k'; break;
  case yur_CH_TAG_l: ret = 'l'; break;
  case yur_CH_TAG_m: ret = 'm'; break;
  case yur_CH_TAG_n: ret = 'n'; break;
  case yur_CH_TAG_o: ret = 'o'; break;
  case yur_CH_TAG_p: ret = 'p'; break;
  case yur_CH_TAG_q: ret = 'q'; break;
  case yur_CH_TAG_r: ret = 'r'; break;
  case yur_CH_TAG_s: ret = 's'; break;
  case yur_CH_TAG_t: ret = 't'; break;
  case yur_CH_TAG_u: ret = 'u'; break;
  case yur_CH_TAG_v: ret = 'v'; break;
  case yur_CH_TAG_w: ret = 'w'; break;
  case yur_CH_TAG_x: ret = 'x'; break;
  case yur_CH_TAG_y: ret = 'y'; break;
  case yur_CH_TAG_z: ret = 'z'; break;
  case yur_CH_TAG_A: ret = 'A'; break;
  case yur_CH_TAG_B: ret = 'B'; break;
  case yur_CH_TAG_C: ret = 'C'; break;
  case yur_CH_TAG_D: ret = 'D'; break;
  case yur_CH_TAG_E: ret = 'E'; break;
  case yur_CH_TAG_F: ret = 'F'; break;
  case yur_CH_TAG_G: ret = 'G'; break;
  case yur_CH_TAG_H: ret = 'H'; break;
  case yur_CH_TAG_I: ret = 'I'; break;
  case yur_CH_TAG_J: ret = 'J'; break;
  case yur_CH_TAG_K: ret = 'K'; break;
  case yur_CH_TAG_L: ret = 'L'; break;
  case yur_CH_TAG_M: ret = 'M'; break;
  case yur_CH_TAG_N: ret = 'N'; break;
  case yur_CH_TAG_O: ret = 'O'; break;
  case yur_CH_TAG_P: ret = 'P'; break;
  case yur_CH_TAG_Q: ret = 'Q'; break;
  case yur_CH_TAG_R: ret = 'R'; break;
  case yur_CH_TAG_S: ret = 'S'; break;
  case yur_CH_TAG_T: ret = 'T'; break;
  case yur_CH_TAG_U: ret = 'U'; break;
  case yur_CH_TAG_V: ret = 'V'; break;
  case yur_CH_TAG_W: ret = 'W'; break;
  case yur_CH_TAG_X: ret = 'X'; break;
  case yur_CH_TAG_Y: ret = 'Y'; break;
  case yur_CH_TAG_Z: ret = 'Z'; break;
  case yur_CH_TAG_null: ret = '\0'; break;
  case yur_CH_TAG_line_feed: ret = '\n'; break;
  case yur_CH_TAG_horizontal_tab: ret = '\t'; break;
  case yur_CH_TAG_space: ret = ' '; break;
  case yur_CH_TAG_double_quote: ret = '"'; break;
  case yur_CH_TAG_pound: ret = '#'; break;
  case yur_CH_TAG_single_quote: ret = '\''; break;
  case yur_CH_TAG_caret: ret = '^'; break;
  case yur_CH_TAG_at: ret = '@'; break;
  case yur_CH_TAG_back_slash: ret = '\\'; break;
  case yur_CH_TAG_forward_slash: ret = '/'; break;
  case yur_CH_TAG_asterisk: ret = '*'; break;
  case yur_CH_TAG_percent: ret = '%'; break;
  case yur_CH_TAG_dollar: ret = '$'; break;
  case yur_CH_TAG_vertical_bar: ret = '|'; break;
  case yur_CH_TAG_ampersand: ret = '&'; break;
  case yur_CH_TAG_plus: ret = '+'; break;
  case yur_CH_TAG_minus: ret = '-'; break;
  case yur_CH_TAG_question_mark: ret = '?'; break;
  case yur_CH_TAG_exclamation_mark: ret = '!'; break;
  case yur_CH_TAG_less_than: ret = '<'; break;
  case yur_CH_TAG_greater_than: ret = '>'; break;
  case yur_CH_TAG_equal: ret = '='; break;
  case yur_CH_TAG_colon: ret = ':'; break;
  case yur_CH_TAG_tilde: ret = '~'; break;
  case yur_CH_TAG_start_of_heading: ret = '\x1'; break;
  case yur_CH_TAG_start_of_text: ret = '\x2'; break;
  case yur_CH_TAG_end_of_text: ret = '\x3'; break;
  case yur_CH_TAG_end_of_transmission: ret = '\x4'; break;
  case yur_CH_TAG_enquiry: ret = '\x5'; break;
  case yur_CH_TAG_acknowledge: ret = '\x6'; break;
  case yur_CH_TAG_bell: ret = '\x7'; break;
  case yur_CH_TAG_back_space: ret = '\x8'; break;
  case yur_CH_TAG_vertival_tab: ret = '\xB'; break;
  case yur_CH_TAG_form_feed: ret = '\xC'; break;
  case yur_CH_TAG_carriage_return: ret = '\xD'; break;
  case yur_CH_TAG_shift_out: ret = '\xE'; break;
  case yur_CH_TAG_shift_in: ret = '\xF'; break;
  case yur_CH_TAG_data_link_escape: ret = '\x10'; break;
  case yur_CH_TAG_device_control_1: ret = '\x11'; break;
  case yur_CH_TAG_device_control_2: ret = '\x12'; break;
  case yur_CH_TAG_device_control_3: ret = '\x13'; break;
  case yur_CH_TAG_device_control_4: ret = '\x14'; break;
  case yur_CH_TAG_negative_acknowledge: ret = '\x15'; break;
  case yur_CH_TAG_synchronous_idle: ret = '\x16'; break;
  case yur_CH_TAG_end_of_transmission_block: ret = '\x17'; break;
  case yur_CH_TAG_cancel: ret = '\x18'; break;
  case yur_CH_TAG_end_of_medium: ret = '\x19'; break;
  case yur_CH_TAG_substitute: ret = '\x1A'; break;
  case yur_CH_TAG_escape: ret = '\x1B'; break;
  case yur_CH_TAG_file_seperator: ret = '\x1C'; break;
  case yur_CH_TAG_group_seperator: ret = '\x1D'; break;
  case yur_CH_TAG_record_seperator: ret = '\x1E'; break;
  case yur_CH_TAG_unit_seperator: ret = '\x1F'; break;
  case yur_CH_TAG_left_parenthesis: ret = '('; break;
  case yur_CH_TAG_right_parenthesis: ret = ')'; break;
  case yur_CH_TAG_left_square_bracket: ret = '['; break;
  case yur_CH_TAG_right_square_bracket: ret = ']'; break;
  case yur_CH_TAG_left_curly_bracket: ret = '{'; break;
  case yur_CH_TAG_right_curly_bracket: ret = '}'; break;
  case yur_CH_TAG_period: ret = '.'; break;
  case yur_CH_TAG_comma: ret = ','; break;
  case yur_CH_TAG_semicolon: ret = ';'; break;
  case yur_CH_TAG_underscore: ret = '_'; break;
  case yur_CH_TAG_grave_accent: ret = '`'; break;
  case yur_CH_TAG_delete: ret = '\x7F'; break;
  default: ret = '\0'; break;
  }
  yur_unref(c);
  return ret;
}

yur_Ref *yur_cchar_to_yuchar(char c) {
  switch (c) {
  case '0': return &yur_ch_0;
  case '1': return &yur_ch_1;
  case '2': return &yur_ch_2;
  case '3': return &yur_ch_3;
  case '4': return &yur_ch_4;
  case '5': return &yur_ch_5;
  case '6': return &yur_ch_6;
  case '7': return &yur_ch_7;
  case '8': return &yur_ch_8;
  case '9': return &yur_ch_9;
  case 'a': return &yur_ch_a;
  case 'b': return &yur_ch_b;
  case 'c': return &yur_ch_c;
  case 'd': return &yur_ch_d;
  case 'e': return &yur_ch_e;
  case 'f': return &yur_ch_f;
  case 'g': return &yur_ch_g;
  case 'h': return &yur_ch_h;
  case 'i': return &yur_ch_i;
  case 'j': return &yur_ch_j;
  case 'k': return &yur_ch_k;
  case 'l': return &yur_ch_l;
  case 'm': return &yur_ch_m;
  case 'n': return &yur_ch_n;
  case 'o': return &yur_ch_o;
  case 'p': return &yur_ch_p;
  case 'q': return &yur_ch_q;
  case 'r': return &yur_ch_r;
  case 's': return &yur_ch_s;
  case 't': return &yur_ch_t;
  case 'u': return &yur_ch_u;
  case 'v': return &yur_ch_v;
  case 'w': return &yur_ch_w;
  case 'x': return &yur_ch_x;
  case 'y': return &yur_ch_y;
  case 'z': return &yur_ch_z;
  case 'A': return &yur_ch_A;
  case 'B': return &yur_ch_B;
  case 'C': return &yur_ch_C;
  case 'D': return &yur_ch_D;
  case 'E': return &yur_ch_E;
  case 'F': return &yur_ch_F;
  case 'G': return &yur_ch_G;
  case 'H': return &yur_ch_H;
  case 'I': return &yur_ch_I;
  case 'J': return &yur_ch_J;
  case 'K': return &yur_ch_K;
  case 'L': return &yur_ch_L;
  case 'M': return &yur_ch_M;
  case 'N': return &yur_ch_N;
  case 'O': return &yur_ch_O;
  case 'P': return &yur_ch_P;
  case 'Q': return &yur_ch_Q;
  case 'R': return &yur_ch_R;
  case 'S': return &yur_ch_S;
  case 'T': return &yur_ch_T;
  case 'U': return &yur_ch_U;
  case 'V': return &yur_ch_V;
  case 'W': return &yur_ch_W;
  case 'X': return &yur_ch_X;
  case 'Y': return &yur_ch_Y;
  case 'Z': return &yur_ch_Z;
  case '\0': return &yur_ch_null;
  case '\n': return &yur_ch_line_feed;
  case '\t': return &yur_ch_horizontal_tab;
  case ' ': return &yur_ch_space;
  case '"': return &yur_ch_double_quote;
  case '#': return &yur_ch_pound;
  case '\'': return &yur_ch_single_quote;
  case '^': return &yur_ch_caret;
  case '@': return &yur_ch_at;
  case '\\': return &yur_ch_back_slash;
  case '/': return &yur_ch_forward_slash;
  case '*': return &yur_ch_asterisk;
  case '%': return &yur_ch_percent;
  case '$': return &yur_ch_dollar;
  case '|': return &yur_ch_vertical_bar;
  case '&': return &yur_ch_ampersand;
  case '+': return &yur_ch_plus;
  case '-': return &yur_ch_minus;
  case '?': return &yur_ch_question_mark;
  case '!': return &yur_ch_exclamation_mark;
  case '<': return &yur_ch_less_than;
  case '>': return &yur_ch_greater_than;
  case '=': return &yur_ch_equal;
  case ':': return &yur_ch_colon;
  case '~': return &yur_ch_tilde;
  case '\x1': return &yur_ch_start_of_heading;
  case '\x2': return &yur_ch_start_of_text;
  case '\x3': return &yur_ch_end_of_text;
  case '\x4': return &yur_ch_end_of_transmission;
  case '\x5': return &yur_ch_enquiry;
  case '\x6': return &yur_ch_acknowledge;
  case '\x7': return &yur_ch_bell;
  case '\x8': return &yur_ch_back_space;
  case '\xB': return &yur_ch_vertival_tab;
  case '\xC': return &yur_ch_form_feed;
  case '\xD': return &yur_ch_carriage_return;
  case '\xE': return &yur_ch_shift_out;
  case '\xF': return &yur_ch_shift_in;
  case '\x10': return &yur_ch_data_link_escape;
  case '\x11': return &yur_ch_device_control_1;
  case '\x12': return &yur_ch_device_control_2;
  case '\x13': return &yur_ch_device_control_3;
  case '\x14': return &yur_ch_device_control_4;
  case '\x15': return &yur_ch_negative_acknowledge;
  case '\x16': return &yur_ch_synchronous_idle;
  case '\x17': return &yur_ch_end_of_transmission_block;
  case '\x18': return &yur_ch_cancel;
  case '\x19': return &yur_ch_end_of_medium;
  case '\x1A': return &yur_ch_substitute;
  case '\x1B': return &yur_ch_escape;
  case '\x1C': return &yur_ch_file_seperator;
  case '\x1D': return &yur_ch_group_seperator;
  case '\x1E': return &yur_ch_record_seperator;
  case '\x1F': return &yur_ch_unit_seperator;
  case '(': return &yur_ch_left_parenthesis;
  case ')': return &yur_ch_right_parenthesis;
  case '[': return &yur_ch_left_square_bracket;
  case ']': return &yur_ch_right_square_bracket;
  case '{': return &yur_ch_left_curly_bracket;
  case '}': return &yur_ch_right_curly_bracket;
  case '.': return &yur_ch_period;
  case ',': return &yur_ch_comma;
  case ';': return &yur_ch_semicolon;
  case '_': return &yur_ch_underscore;
  case '`': return &yur_ch_grave_accent;
  case '\x7F': return &yur_ch_delete;
  default: return &yur_ch_null;
  }
}

char *yur_yustr_to_cstr(yur_Ref *s0) {
  yur_Ref *s = s0->fields[0];
  yur_inc(s);
  yur_inc(s);
  const size_t n = yur_yulist_csize(s);
  char *r = yur_cmalloc(n + 1);
  yur_Ref *x = s;
  for (size_t i = 0; i < n; i++) {
    yur_Ref *c = x->fields[0];
    yur_inc(c);
    r[i] = yur_yuchar_to_cchar(c);
    x = x->fields[1];
  }
  yur_unref(s0);
  yur_unref(s);
  r[n] = '\0';
  return r;
}

static yur_Ref *yur_cstr_to_yustr_aux(const char *s) {
  const char c = *s;
  if (!c)
    return &yur_unit;
  yur_Ref *n = yur_build(2, 1);
  n->fields[0] = yur_cchar_to_yuchar(c);
  n->fields[1] = yur_cstr_to_yustr_aux(s + 1);
  return n;
}

yur_Ref *yur_cstr_to_yustr(const char *s) {
  yur_Ref *r = yur_build(1, 0);
  r->fields[0] = yur_cstr_to_yustr_aux(s);
  return r;
}

yur_Ref *
yu__doprint_doyu_slprint_slprint_nuChar_doyu_slChar_slType(yur_Ref *x) {
  yur_putchar(yur_yuchar_to_cchar(x));
  return &yur_unit;
}

yur_Ref *yu__docmp_doyu_slChar_slChar_nuChar_doyu_slChar_slType(yur_Ref *c, yur_Ref *b) {
  size_t t;
  if (b->tag < c->tag) {
    t = 0;
  } else if (c->tag < b->tag) {
    t = 2;
  } else {
    t = 1;
  }
  yur_unref(b);
  yur_unref(c);
  return yur_build(0, t);
}

yur_Ref *yu_cmpIsTransitive_doyu_slChar_slChar(yur_Ref *r2, yur_Ref *r1,
    yur_Ref *z, yur_Ref *y, yur_Ref *x) {
  yur_unref(x);
  yur_unref(y);
  yur_unref(z);
  yur_unref(r1);
  yur_unref(r2);
  yur_Ref *e = yur_build(1, 0);
  e->fields[0] = yur_build(0, 1);
  return e;
}

static yur_Ref bool_true = {
  .count = 0,
  .tag = 1,
  .vmt_index = yur_Static_vmt,
  .nfields = 0
};

static yur_Ref *is_lt(yur_Ref *x) {
  yur_Ref *r = x->tag ? &yur_unit : &bool_true;
  yur_unref(x);
  return r;
}

yur_Ref *yu_cmpIsCompatible_doyu_slChar_slChar(yur_Ref *c, yur_Ref *b) {
  yur_Ref *e = yur_build(1, 0);
  e->fields[0] = is_lt(yu__docmp_doyu_slChar_slChar_nuChar_doyu_slChar_slType(b, c));
  return e;
}
