Newspeak output
---------------
526.c
void main(void) {
  (526.c:27#1072)^ptr ptr;
  (526.c:28#1084)^int32 i;
  int32 tmp;
  (526.c:30#1092)^tmp =(int32) i_int32;
  (526.c:30#1092)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
  (526.c:30#1092)^[(ptr_ptr + (tmp_int32 * 32))]32 =(int32) [(ptr_ptr + (tmp_int32 * 32))]32_int32;
}


