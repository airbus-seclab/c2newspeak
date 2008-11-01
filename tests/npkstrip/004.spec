Warning: unknown function call_fptr called: assuming it does not call any function, strip may be incorrect in 004.c line 33
004.c
main() {
  (004.c:33#1140)^fptr call_fptr.arg1;
  (004.c:33#1140)^0- =(fptr) &_{int32 -> int32}(g);
  (004.c:33#1140)^call_fptr();
}

