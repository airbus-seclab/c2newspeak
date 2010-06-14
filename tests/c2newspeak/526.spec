Warning: 526.c:30#2: expression without post effects expected
Newspeak output
---------------
526.c
void main(void) {
  (526.c:27#7)^ptr ptr;
  (526.c:28#6)^int32 i;
  (526.c:30#2)^[(ptr_ptr + (i_int32 * 32))]32 =(int32) ([(ptr_ptr + (i_int32 * 32))]32_int32 | 0);
  (526.c:30#2)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
}


