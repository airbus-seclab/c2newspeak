Warning: unknown function call_fptr called: assuming it does not call any function, strip may be incorrect in 004.c line 33
004.c
int32 g(int32) {
  (004.c:29#2)^1- =(int32) 0-_int32;
}

void main(void) {
  (004.c:33#2)^fptr call_fptr.arg1;
  (004.c:33#2)^0- =(fptr) &_{int32 -> int32}(g);
  (004.c:33#2)^call_fptr();
}

