Warning: unknown function call_fptr called: assuming it does not call any function, strip may be incorrect in 004.c line 29
int32 (004.c:28#4)^g(int32 x) {
  (004.c:29#2)^!return =(int32) x_int32;
}

void (004.c:32#5)^main(void) {
  (004.c:33#2)^call_fptr(&_{(int32) -> int32}(g): fptr);
}

