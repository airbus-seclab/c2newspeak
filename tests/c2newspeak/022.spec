Newspeak output
---------------
022.c
void main(void) {
  (022.c:30#6)^int32 a;
  (022.c:30#9)^int32 b;
  (022.c:30#12)^int32 c;
  (022.c:31#2)^choose {
   -->
    (022.c:31#2)^guard(! (a_int32 ==_int32 0));
    (022.c:31#2)^guard(! (b_int32 ==_int32 0));
    (022.c:32#4)^a =(int32) 1;
   -->
    (022.c:31#2)^choose {
     -->
      (022.c:31#2)^guard(! (a_int32 ==_int32 0));
      (022.c:31#2)^guard((b_int32 ==_int32 0));
     -->
      (022.c:31#2)^guard((a_int32 ==_int32 0));
    }
    (022.c:33#9)^choose {
     -->
      (022.c:33#9)^guard(! (c_int32 ==_int32 0));
      (022.c:34#4)^b =(int32) 2;
     -->
      (022.c:33#9)^guard((c_int32 ==_int32 0));
    }
  }
}


