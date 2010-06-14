Newspeak output
---------------
373.c
void main(void) {
  (373.c:28#6)^int32 x;
  (373.c:28#9)^int32 y;
  (373.c:29#2)^int32 !tmp0;
  (373.c:29#2)^!tmp0 <- f();
  (373.c:29#2)^choose {
   -->
    (373.c:29#2)^guard(! (!tmp0_int32 ==_int32 0));
    (373.c:29#2)^choose {
     -->
      (373.c:29#2)^guard(! (x_int32 ==_int32 0));
     -->
      (373.c:29#2)^guard((x_int32 ==_int32 0));
      (373.c:29#2)^guard(! (y_int32 ==_int32 0));
    }
    (373.c:30#4)^x =(int32) 0;
    (373.c:31#4)^x =(int32) 0;
   -->
    (373.c:29#2)^choose {
     -->
      (373.c:29#2)^guard(! (!tmp0_int32 ==_int32 0));
      (373.c:29#2)^guard((x_int32 ==_int32 0));
      (373.c:29#2)^guard((y_int32 ==_int32 0));
     -->
      (373.c:29#2)^guard((!tmp0_int32 ==_int32 0));
    }
    (373.c:33#4)^x =(int32) 1;
    (373.c:34#4)^x =(int32) 1;
  }
}


