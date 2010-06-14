Newspeak output
---------------
226.c
void main(void) {
  (226.c:27#1072)^int32 x;
  (226.c:28#1082)^fptr[10] fptr;
  int32 tmp;
  (226.c:29#1104)^tmp =(int32) x_int32;
  (226.c:29#1104)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (226.c:29#1104)^[fptr + (belongs[0,9] tmp_int32 * 32)_fptr]();
}


