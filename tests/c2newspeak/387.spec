Warning: 387.c:29#0: deprecated style of function definition accepted
Warning: 387.c:36#0: deprecated style of function definition accepted
Newspeak output
---------------
int32 (387.c:26#4)^f(int8 a, int32 b) {
  (387.c:30#2)^!return =(int32) coerce[-2147483648,2147483647] (a_int8 + b_int32);
}

int32 (387.c:33#4)^g(int8 a, int32 b) {
  (387.c:37#2)^!return =(int32) coerce[-2147483648,2147483647] (a_int8 + b_int32);
}

int32 (387.c:40#4)^h(int8 a, int32 b) {
  (387.c:41#2)^!return =(int32) coerce[-2147483648,2147483647] (a_int8 + b_int32);
}


