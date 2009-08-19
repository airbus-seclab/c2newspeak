Warning: 387.c:29#0: deprecated style of function definition accepted
Warning: 387.c:36#0: deprecated style of function definition accepted
Newspeak output
---------------
387.c
int32 f(int8, int32) {
  (387.c:30#2)^2- =(int32) coerce[-2147483648,2147483647] (1-_int8 + 0-_int32);
}

int32 g(int8, int32) {
  (387.c:37#2)^2- =(int32) coerce[-2147483648,2147483647] (1-_int8 + 0-_int32);
}

int32 h(int8, int32) {
  (387.c:41#2)^2- =(int32) coerce[-2147483648,2147483647] (1-_int8 + 0-_int32);
}


