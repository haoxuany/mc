#include "stdio.h"
#include "stdlib.h"

void f_2(size_t*, size_t*, size_t*, size_t*);

void fn_0(size_t* arg_0, size_t* arg_1) {
  exit(0);
}

void fn_1(size_t* arg_2, size_t* arg_3) {
  exit(1);
}

void f_2(size_t* arg_4, size_t* arg_5, size_t* arg_6, size_t* arg_7) {
  size_t* tuple_0 = ((void*)malloc(16));
  size_t* tuple_1;
  tuple_0[0] = tuple_1;
  tuple_0[1] = f_2;
  size_t* letbound_0 = tuple_0;
  size_t* letbound_1 = letbound_0;
  size_t* projected_0 = letbound_1[1];
  size_t* projected_1 = letbound_1[0];
  ((void (*)(size_t*, size_t*, size_t*, size_t*))projected_0)(arg_4, arg_5, arg_6, projected_1);
}

void fn_2(size_t* arg_8, size_t* arg_9, size_t* arg_10, size_t* arg_11) {
  size_t* letbound_2 = arg_11;
  size_t* projected_2 = letbound_2[0];
  size_t* projected_3 = letbound_2[1];
  ((void (*)(size_t*, size_t*, size_t*, size_t*))f_2)(arg_8, arg_9, arg_10, projected_2);
}

void main() {
  size_t* tuple_2 = ((void*)malloc(16));
  size_t* tuple_3 = ((void*)malloc(16));
  size_t* tuple_4;
  tuple_3[0] = tuple_4;
  tuple_3[1] = NULL;
  tuple_2[0] = tuple_3;
  tuple_2[1] = fn_2;
  size_t* letbound_3 = tuple_2;
  size_t* projected_4 = letbound_3[1];
  size_t* projected_5 = letbound_3[0];
  size_t* tuple_5;
  size_t* tuple_6 = ((void*)malloc(16));
  size_t* tuple_7;
  tuple_6[0] = tuple_7;
  tuple_6[1] = fn_0;
  size_t* tuple_8 = ((void*)malloc(16));
  size_t* tuple_9;
  tuple_8[0] = tuple_9;
  tuple_8[1] = fn_1;
  ((void (*)(size_t*, size_t*, size_t*, size_t*))projected_4)(tuple_5, tuple_6, tuple_8, projected_5);
}
