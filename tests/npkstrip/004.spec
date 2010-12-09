Warning: unknown function call_fptr called: assuming it does not call any function, strip may be incorrect in 004.c line 29
004.c
int32 g(int32 x) {
  (004.c:29#2)^!return =(int32) x_int32;
}

void main(void) {
  (004.c:33#2)^call_fptr(&_{(int32) -> int32}(g): fptr);
}

