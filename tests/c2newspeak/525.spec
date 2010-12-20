Warning: 525.c:29#0: comma in expression accepted
Warning: 525.c:29#0: block within expression accepted
Warning: 525.c:29#28: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
void main(void) {
  (525.c:27#6)^int32 x;
  (525.c:28#6)^int32 y;
  (525.c:29#2)^choose {
   -->
    (525.c:29#2)^guard(! (x_int32 ==_int32 0));
    (525.c:29#2)^x =(int32) 1;
   -->
    (525.c:29#2)^guard((x_int32 ==_int32 0));
    (525.c:29#28)^y =(int32) 1;
    (525.c:29#28)^{
      int32 tmp_firstpass!0;
      (525.c:29#28)^choose {
       -->
        (525.c:29#28)^guard(! (x_int32 ==_int32 0));
        (525.c:29#28)^tmp_firstpass!0 =(int32) 1;
       -->
        (525.c:29#28)^guard((x_int32 ==_int32 0));
        (525.c:29#28)^tmp_firstpass!0 =(int32) 0;
      }
      (525.c:29#2)^x =(int32) tmp_firstpass!0_int32;
    }
  }
}


