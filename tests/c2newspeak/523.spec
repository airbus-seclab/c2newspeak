Warning: 523.c:28#0: block within expression accepted
Newspeak output
---------------
523.c
void main(void) {
  (523.c:27#6)^int32 x;
  (523.c:28#2)^int32 tmp0;
  (523.c:28#2)^choose {
   -->
    (523.c:28#2)^guard(! (x_int32 ==_int32 0));
    (523.c:28#2)^tmp0 =(int32) 1;
   -->
    (523.c:28#2)^guard((x_int32 ==_int32 0));
    (523.c:28#2)^tmp0 =(int32) 0;
  }
  (523.c:28#2)^x =(int32) tmp0_int32;
}


