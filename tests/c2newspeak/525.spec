Warning: 525.c:29#1084: comma in expression accepted
Warning: 525.c:29#1084: block within expression accepted
Newspeak output
---------------
525.c
void main(void) {
  (525.c:27#6)^int32 x;
  (525.c:28#6)^int32 y;
  (525.c:29#2)^choose {
   -->
    (525.c:29#2)^guard(! (1-_int32 ==_int32 0));
    (525.c:29#2)^1- =(int32) 1;
   -->
    (525.c:29#2)^guard((1-_int32 ==_int32 0));
    (525.c:29#28)^0- =(int32) 1;
    (525.c:29#28)^{
      int32 !tmp0;
      (525.c:29#28)^choose {
       -->
        (525.c:29#28)^guard(! (2-_int32 ==_int32 0));
        (525.c:29#28)^0- =(int32) 1;
       -->
        (525.c:29#28)^guard((2-_int32 ==_int32 0));
        (525.c:29#28)^0- =(int32) 0;
      }
      (525.c:29#2)^2- =(int32) 0-_int32;
    }
  }
}


