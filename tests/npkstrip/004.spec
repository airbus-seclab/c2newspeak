Warning: unknown function call_fptr called: assuming it does not call any function, strip may be incorrect in 004.c line 33
004.c
void main(void) {
  (004.c:33#2)^fptr call_fptr.arg1;
  (004.c:33#2)^0- =(fptr) &_{int32 -> int32}(g);
  (004.c:33#2)^call_fptr();
}

