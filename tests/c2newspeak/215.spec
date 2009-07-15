Newspeak output
---------------
215.c
int32 f(void) {
  (215.c:30#2)^0- =(int32) 4;
}

void main(void) {
  (215.c:34#6)^int32[10] t;
  (215.c:36#2)^int32 !tmp0;
  (215.c:36#2)^0- =(int32) 1- + (belongs[0,9] x_int32 * 32)_int32;
  (215.c:36#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (215.c:36#2)^f();
}

int32 x;
(215.c:26#4)^x =(int32) 1;

