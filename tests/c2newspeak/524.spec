Warning: 524.c:29#0: comma in expression accepted
Warning: 524.c:29#0: block within expression accepted
Warning: 524.c:29#24: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
void main(void) {
  (524.c:27#6)^int32 x;
  (524.c:28#6)^int32 y;
  (524.c:29#24)^y =(int32) 1;
  (524.c:29#24)^{
    int32 tmp_firstpass!0;
    (524.c:29#24)^choose {
     -->
      (524.c:29#24)^guard(! (x_int32 ==_int32 0));
      (524.c:29#24)^tmp_firstpass!0 =(int32) 1;
     -->
      (524.c:29#24)^guard((x_int32 ==_int32 0));
      (524.c:29#24)^tmp_firstpass!0 =(int32) 0;
    }
    (524.c:29#2)^x =(int32) tmp_firstpass!0_int32;
  }
}


