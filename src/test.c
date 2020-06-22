#include "stdio.h"
#include "stdlib.h"

void fn_0(size_t* arg_0, size_t* arg_1, size_t* arg_2, size_t* arg_3) {
  size_t* letbound_0;
  letbound_0 = arg_3;
  size_t* projected_0;
  projected_0 = letbound_0[0];
  size_t* projected_1;
  projected_1 = letbound_0[1];
  ((void (*)(size_t*, size_t*, size_t*, size_t*))f_1)(arg_0, arg_1, arg_2, projected_0);
}
void fn_1(size_t* arg_4, size_t* arg_5) {
  size_t* projected_2;
  projected_2 = arg_5[0];
  size_t* projected_3;
  projected_3 = arg_5[1];
  size_t* projected_4;
  projected_4 = arg_5[2];
  size_t* letbound_1;
  letbound_1 = projected_4;
  size_t* projected_5;
  projected_5 = letbound_1[1];
  size_t* projected_6;
  projected_6 = letbound_1[0];
  ((void (*)(size_t*, size_t*, size_t*, size_t*))projected_5)(arg_4, projected_2, projected_3, projected_6);
}
void f_1(size_t*, size_t*, size_t*, size_t*);
void f_1(size_t* arg_6, size_t* arg_7, size_t* arg_8, size_t* arg_9) {
  size_t* tuple_0;
  tuple_0 = ((size_t*)malloc(16));
  size_t* tuple_1;
  tuple_1 = ((size_t*)malloc(0));
  tuple_0[0] = tuple_1;
  tuple_0[1] = f_1;
  size_t* letbound_2;
  letbound_2 = tuple_0;
  size_t* letbound_3;
  letbound_3 = arg_7;
  size_t* projected_7;
  projected_7 = letbound_3[1];
  size_t* projected_8;
  projected_8 = letbound_3[0];
  ((void (*)(size_t*, size_t*))projected_7)(arg_6, projected_8);
}
void fn_3(size_t* arg_10, size_t* arg_11) {
  size_t* projected_9;
  projected_9 = arg_11[0];
  size_t* letbound_4;
  letbound_4 = projected_9;
  size_t* projected_10;
  projected_10 = letbound_4[1];
  size_t* projected_11;
  projected_11 = letbound_4[0];
  size_t* tuple_2;
  tuple_2 = ((size_t*)malloc(16));
  tuple_2[0] = arg_10;
  tuple_2[1] = fn_0;
  ((void (*)(size_t*, size_t*))projected_10)(tuple_2, projected_11);
}
void fn_4(size_t* arg_12, size_t* arg_13) {
  size_t* projected_12;
  projected_12 = arg_13[0];
  size_t* projected_13;
  projected_13 = arg_13[1];
  size_t* tuple_3;
  tuple_3 = ((size_t*)malloc(16));
  size_t* tuple_4;
  tuple_4 = ((size_t*)malloc(24));
  tuple_4[0] = projected_12;
  tuple_4[1] = projected_13;
  tuple_4[2] = arg_12;
  tuple_3[0] = tuple_4;
  tuple_3[1] = fn_1;
  size_t* letbound_5;
  letbound_5 = tuple_3;
  size_t* letbound_6;
  letbound_6 = letbound_5;
  size_t* projected_14;
  projected_14 = letbound_6[1];
  size_t* projected_15;
  projected_15 = letbound_6[0];
  size_t* tuple_5;
  tuple_5 = ((size_t*)malloc(0));
  ((void (*)(size_t*, size_t*))projected_14)(tuple_5, projected_15);
}
void fn_5(size_t* arg_14, size_t* arg_15) {
  exit(1);
}
void fn_6(size_t* arg_16, size_t* arg_17) {
  exit(0);
}
void main() {
  size_t* tuple_6;
  tuple_6 = ((size_t*)malloc(16));
  size_t* tuple_7;
  tuple_7 = ((size_t*)malloc(0));
  tuple_6[0] = tuple_7;
  tuple_6[1] = fn_6;
  size_t* letbound_7;
  letbound_7 = tuple_6;
  size_t* tuple_8;
  tuple_8 = ((size_t*)malloc(16));
  size_t* tuple_9;
  tuple_9 = ((size_t*)malloc(0));
  tuple_8[0] = tuple_9;
  tuple_8[1] = fn_5;
  size_t* letbound_8;
  letbound_8 = tuple_8;
  size_t* tuple_10;
  tuple_10 = ((size_t*)malloc(16));
  size_t* tuple_11;
  tuple_11 = ((size_t*)malloc(16));
  tuple_11[0] = letbound_7;
  tuple_11[1] = letbound_8;
  tuple_10[0] = tuple_11;
  tuple_10[1] = fn_4;
  size_t* letbound_9;
  letbound_9 = tuple_10;
  size_t* tuple_12;
  tuple_12 = ((size_t*)malloc(16));
  size_t* tuple_13;
  tuple_13 = ((size_t*)malloc(8));
  tuple_13[0] = letbound_9;
  tuple_12[0] = tuple_13;
  tuple_12[1] = fn_3;
  size_t* letbound_10;
  letbound_10 = tuple_12;
  size_t* letbound_11;
  letbound_11 = letbound_10;
  size_t* projected_16;
  projected_16 = letbound_11[1];
  size_t* projected_17;
  projected_17 = letbound_11[0];
  size_t* tuple_14;
  tuple_14 = ((size_t*)malloc(16));
  size_t* tuple_15;
  tuple_15 = ((size_t*)malloc(0));
  tuple_14[0] = tuple_15;
  tuple_14[1] = fn_2;
  ((void (*)(size_t*, size_t*))projected_16)(tuple_14, projected_17);
}
