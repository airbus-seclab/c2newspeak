Warning: 226.c:29#2: expression of type signed integer used as an array index accepted
Newspeak output
---------------
226.c
void main(void) {
  (226.c:27#6)^int32 x;
  (226.c:28#9)^fptr[10] fptr;
  (226.c:29#2)^fptr tmp_cir!0;
  (226.c:29#2)^tmp_cir!0 =(fptr) fptr + (belongs[0,9] x_int32 * 32)_fptr;
  (226.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (226.c:29#2)^[tmp_cir!0_fptr]();
}


