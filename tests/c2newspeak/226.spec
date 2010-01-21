Warning: 226.c:29#2: expression of type signed integer used as an array index ignored
Newspeak output
---------------
226.c
void main(void) {
  (226.c:27#6)^int32 x;
  (226.c:28#9)^fptr[10] fptr;
  (226.c:29#2)^fptr !tmp0;
  (226.c:29#2)^0- =(fptr) 1- + (belongs[0,9] 2-_int32 * 32)_fptr;
  (226.c:29#2)^2- =(int32) coerce[-2147483648,2147483647] (2-_int32 + 1);
  (226.c:29#2)^[0-_fptr]();
}


