Warning: 215.c:36#2: expression of type signed integer used as an array index accepted
Newspeak output
---------------
int32 (215.c:28#4)^f(void) {
  (215.c:30#2)^!return =(int32) 4;
}

void (215.c:33#5)^main(void) {
  (215.c:34#6)^int32[10] t;
  (215.c:36#2)^int32 tmp_cir!0;
  (215.c:36#2)^tmp_cir!0 =(int32) t + (belongs[0,9] x_int32 * 32)_int32;
  (215.c:36#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (215.c:36#2)^tmp_cir!0: int32 <- f();
}

int32 x;
(215.c:26#4)^x =(int32) 1;

