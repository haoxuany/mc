#include "stdio.h"
#include "stdlib.h"

void f_3(size_t*, size_t*, size_t*, size_t*);

void fn_0(size_t* arg_0, size_t* arg_1) {
  size_t* projected_0 = arg_1[0];
  size_t* letbound_0 = projected_0;
  size_t* projected_1 = letbound_0[1];
  size_t* projected_2 = letbound_0[0];
  size_t* inj_0 = ((void*)malloc(16));
  inj_0[0] = 0;
  inj_0[1] = arg_0;
  ((void (*)(size_t*, size_t*))projected_1)(inj_0, projected_2);
}

void fn_1(size_t* arg_2, size_t* arg_3) {
  size_t* projected_3 = arg_3[0];
  size_t* letbound_1 = projected_3;
  size_t* projected_4 = letbound_1[1];
  size_t* projected_5 = letbound_1[0];
  size_t* inj_1 = ((void*)malloc(16));
  inj_1[0] = 1;
  inj_1[1] = arg_2;
  ((void (*)(size_t*, size_t*))projected_4)(inj_1, projected_5);
}

void fn_2(size_t* arg_4, size_t* arg_5) {
  size_t* projected_6 = arg_5[0];
  size_t cased_0;
  switch (arg_4[0]) {
  case 0:
    cased_0 = arg_4[1];
    size_t* tuple_0 = ((void*)malloc(16));
    size_t* tuple_1 = ((void*)malloc(8));
    tuple_1[0] = projected_6;
    tuple_0[0] = tuple_1;
    tuple_0[1] = fn_0;
    size_t* letbound_2 = tuple_0;
    size_t* letbound_3 = letbound_2;
    size_t* projected_7 = letbound_3[1];
    size_t* projected_8 = letbound_3[0];
    size_t* tuple_2;
    ((void (*)(size_t*, size_t*))projected_7)(tuple_2, projected_8);
    return;
  case 1:
    cased_0 = arg_4[1];
    size_t* tuple_3 = ((void*)malloc(16));
    size_t* tuple_4 = ((void*)malloc(8));
    tuple_4[0] = projected_6;
    tuple_3[0] = tuple_4;
    tuple_3[1] = fn_1;
    size_t* letbound_4 = tuple_3;
    size_t* letbound_5 = letbound_4;
    size_t* projected_9 = letbound_5[1];
    size_t* projected_10 = letbound_5[0];
    size_t* tuple_5;
    ((void (*)(size_t*, size_t*))projected_9)(tuple_5, projected_10);
    return;}
}

void fn_3(size_t* arg_6, size_t* arg_7, size_t* arg_8, size_t* arg_9) {
  size_t* letbound_6 = arg_9;
  size_t* projected_11 = letbound_6[0];
  size_t* projected_12 = letbound_6[1];
  ((void (*)(size_t*, size_t*, size_t*, size_t*))f_3)(arg_6, arg_7, arg_8, projected_11);
}

void fn_4(size_t* arg_10, size_t* arg_11) {
  size_t* projected_13 = arg_11[0];
  size_t* projected_14 = arg_10[0];
  size_t* letbound_7 = projected_13;
  size_t* projected_15 = letbound_7[1];
  size_t* projected_16 = letbound_7[0];
  ((void (*)(size_t*, size_t*))projected_15)(projected_14, projected_16);
}

void fn_5(size_t* arg_12, size_t* arg_13) {
  size_t* projected_17 = arg_13[0];
  size_t* projected_18 = arg_13[1];
  size_t* projected_19 = arg_13[2];
  size_t* letbound_8 = projected_19;
  size_t* projected_20 = letbound_8[1];
  size_t* projected_21 = letbound_8[0];
  ((void (*)(size_t*, size_t*, size_t*, size_t*))projected_20)(arg_12, projected_17, projected_18, projected_21);
}

void f_3(size_t* arg_14, size_t* arg_15, size_t* arg_16, size_t* arg_17) {
  size_t* tuple_6 = ((void*)malloc(16));
  size_t* tuple_7;
  tuple_6[0] = tuple_7;
  tuple_6[1] = f_3;
  size_t* letbound_9 = tuple_6;
  size_t* tuple_8 = ((void*)malloc(16));
  size_t* tuple_9 = ((void*)malloc(8));
  tuple_9[0] = arg_15;
  tuple_8[0] = tuple_9;
  tuple_8[1] = fn_2;
  size_t* letbound_10 = tuple_8;
  size_t* letbound_11 = letbound_10;
  size_t* projected_22 = letbound_11[1];
  size_t* projected_23 = letbound_11[0];
  ((void (*)(size_t*, size_t*))projected_22)(arg_14, projected_23);
}

void fn_6(size_t* arg_18, size_t* arg_19) {
  size_t* projected_24 = arg_19[0];
  size_t* projected_25 = arg_18[1];
  size_t* letbound_12 = projected_24;
  size_t* projected_26 = letbound_12[1];
  size_t* projected_27 = letbound_12[0];
  ((void (*)(size_t*, size_t*))projected_26)(projected_25, projected_27);
}

void fn_7(size_t* arg_20, size_t* arg_21) {
  size_t* projected_28 = arg_21[0];
  size_t* letbound_13 = projected_28;
  size_t* projected_29 = letbound_13[1];
  size_t* projected_30 = letbound_13[0];
  size_t* tuple_10 = ((void*)malloc(16));
  tuple_10[0] = arg_20;
  tuple_10[1] = fn_3;
  ((void (*)(size_t*, size_t*))projected_29)(tuple_10, projected_30);
}

void fn_8(size_t* arg_22, size_t* arg_23) {
  size_t* projected_31 = arg_23[0];
  size_t* projected_32 = arg_23[1];
  size_t* letbound_14 = projected_31;
  size_t* projected_33 = letbound_14[1];
  size_t* projected_34 = letbound_14[0];
  size_t* tuple_11 = ((void*)malloc(16));
  tuple_11[0] = projected_32;
  tuple_11[1] = arg_22;
  ((void (*)(size_t*, size_t*))projected_33)(tuple_11, projected_34);
}

void fn_9(size_t* arg_24, size_t* arg_25) {
  size_t* projected_35 = arg_25[0];
  size_t* projected_36 = arg_25[1];
  size_t* projected_37 = arg_25[2];
  size_t* tuple_12 = ((void*)malloc(16));
  size_t* tuple_13 = ((void*)malloc(24));
  tuple_13[0] = projected_36;
  tuple_13[1] = projected_37;
  tuple_13[2] = arg_24;
  tuple_12[0] = tuple_13;
  tuple_12[1] = fn_5;
  size_t* letbound_15 = tuple_12;
  size_t* tuple_14 = ((void*)malloc(16));
  size_t* tuple_15 = ((void*)malloc(8));
  tuple_15[0] = letbound_15;
  tuple_14[0] = tuple_15;
  tuple_14[1] = fn_4;
  size_t* letbound_16 = tuple_14;
  size_t* letbound_17 = letbound_16;
  size_t* projected_38 = letbound_17[1];
  size_t* projected_39 = letbound_17[0];
  ((void (*)(size_t*, size_t*))projected_38)(projected_35, projected_39);
}

void fn_10(size_t* arg_26, size_t* arg_27) {
  size_t* projected_40 = arg_27[0];
  size_t* letbound_18 = projected_40;
  size_t* projected_41 = letbound_18[1];
  size_t* projected_42 = letbound_18[0];
  size_t* inj_2 = ((void*)malloc(16));
  inj_2[0] = 0;
  inj_2[1] = arg_26;
  ((void (*)(size_t*, size_t*))projected_41)(inj_2, projected_42);
}

void fn_11(size_t* arg_28, size_t* arg_29) {
  size_t* projected_43 = arg_29[0];
  size_t* tuple_16 = ((void*)malloc(16));
  size_t* tuple_17 = ((void*)malloc(16));
  tuple_17[0] = projected_43;
  tuple_17[1] = arg_28;
  tuple_16[0] = tuple_17;
  tuple_16[1] = fn_8;
  size_t* letbound_19 = tuple_16;
  size_t* letbound_20 = letbound_19;
  size_t* projected_44 = letbound_20[1];
  size_t* projected_45 = letbound_20[0];
  size_t* tuple_18 = ((void*)malloc(16));
  size_t* tuple_19;
  tuple_18[0] = tuple_19;
  tuple_18[1] = NULL;
  ((void (*)(size_t*, size_t*))projected_44)(tuple_18, projected_45);
}

void fn_12(size_t* arg_30, size_t* arg_31) {
  size_t* projected_46 = arg_31[0];
  size_t* letbound_21 = projected_46;
  size_t* projected_47 = letbound_21[1];
  size_t* projected_48 = letbound_21[0];
  ((void (*)(size_t*, size_t*))projected_47)(arg_30, projected_48);
}

void fn_13(size_t* arg_32, size_t* arg_33) {
  size_t* projected_49 = arg_33[0];
  size_t* projected_50 = arg_33[1];
  size_t* letbound_22 = arg_32;
  size_t* tuple_20 = ((void*)malloc(16));
  size_t* tuple_21 = ((void*)malloc(24));
  tuple_21[0] = letbound_22;
  tuple_21[1] = projected_49;
  tuple_21[2] = projected_50;
  tuple_20[0] = tuple_21;
  tuple_20[1] = fn_9;
  size_t* letbound_23 = tuple_20;
  size_t* tuple_22 = ((void*)malloc(16));
  size_t* tuple_23 = ((void*)malloc(8));
  tuple_23[0] = letbound_23;
  tuple_22[0] = tuple_23;
  tuple_22[1] = fn_7;
  size_t* letbound_24 = tuple_22;
  size_t* tuple_24 = ((void*)malloc(16));
  size_t* tuple_25 = ((void*)malloc(8));
  tuple_25[0] = letbound_24;
  tuple_24[0] = tuple_25;
  tuple_24[1] = fn_6;
  size_t* letbound_25 = tuple_24;
  size_t* letbound_26 = letbound_25;
  size_t* projected_51 = letbound_26[1];
  size_t* projected_52 = letbound_26[0];
  ((void (*)(size_t*, size_t*))projected_51)(letbound_22, projected_52);
}

void fn_14(size_t* arg_34, size_t* arg_35) {
  exit(1);
}

void fn_15(size_t* arg_36, size_t* arg_37) {
  exit(0);
}

void main() {
  size_t* tuple_26 = ((void*)malloc(16));
  size_t* tuple_27;
  tuple_26[0] = tuple_27;
  tuple_26[1] = fn_15;
  size_t* letbound_27 = tuple_26;
  size_t* tuple_28 = ((void*)malloc(16));
  size_t* tuple_29;
  tuple_28[0] = tuple_29;
  tuple_28[1] = fn_14;
  size_t* letbound_28 = tuple_28;
  size_t* tuple_30 = ((void*)malloc(16));
  size_t* tuple_31 = ((void*)malloc(16));
  tuple_31[0] = letbound_27;
  tuple_31[1] = letbound_28;
  tuple_30[0] = tuple_31;
  tuple_30[1] = fn_13;
  size_t* letbound_29 = tuple_30;
  size_t* tuple_32 = ((void*)malloc(16));
  size_t* tuple_33 = ((void*)malloc(8));
  tuple_33[0] = letbound_29;
  tuple_32[0] = tuple_33;
  tuple_32[1] = fn_12;
  size_t* letbound_30 = tuple_32;
  size_t* tuple_34 = ((void*)malloc(16));
  size_t* tuple_35 = ((void*)malloc(8));
  tuple_35[0] = letbound_30;
  tuple_34[0] = tuple_35;
  tuple_34[1] = fn_11;
  size_t* letbound_31 = tuple_34;
  size_t* tuple_36 = ((void*)malloc(16));
  size_t* tuple_37 = ((void*)malloc(8));
  tuple_37[0] = letbound_31;
  tuple_36[0] = tuple_37;
  tuple_36[1] = fn_10;
  size_t* letbound_32 = tuple_36;
  size_t* letbound_33 = letbound_32;
  size_t* projected_53 = letbound_33[1];
  size_t* projected_54 = letbound_33[0];
  size_t* tuple_38;
  ((void (*)(size_t*, size_t*))projected_53)(tuple_38, projected_54);
}
