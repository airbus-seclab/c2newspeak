Newspeak output
---------------
505.c
void main(void) {
  (505.c:29#2)^choose {
   -->
    (505.c:29#2)^choose {
     -->
      (505.c:29#2)^guard(! (a_int32 ==_int32 0));
      (505.c:29#2)^choose {
       -->
        (505.c:29#2)^guard(! (b_int32 ==_int32 0));
       -->
        (505.c:29#2)^guard((b_int32 ==_int32 0));
        (505.c:29#2)^guard(! (c_int32 ==_int32 0));
        (505.c:29#2)^guard(! (d_int32 ==_int32 0));
      }
     -->
      (505.c:29#2)^guard((a_int32 ==_int32 0));
      (505.c:29#2)^guard(! (c_int32 ==_int32 0));
      (505.c:29#2)^guard(! (d_int32 ==_int32 0));
    }
    (505.c:30#4)^x =(int32) 0;
    (505.c:31#4)^x =(int32) 3;
   -->
    (505.c:29#2)^choose {
     -->
      (505.c:29#2)^guard(! (a_int32 ==_int32 0));
      (505.c:29#2)^guard((b_int32 ==_int32 0));
      (505.c:29#2)^choose {
       -->
        (505.c:29#2)^guard(! (c_int32 ==_int32 0));
        (505.c:29#2)^guard((d_int32 ==_int32 0));
       -->
        (505.c:29#2)^guard((c_int32 ==_int32 0));
      }
     -->
      (505.c:29#2)^guard((a_int32 ==_int32 0));
      (505.c:29#2)^choose {
       -->
        (505.c:29#2)^guard(! (c_int32 ==_int32 0));
        (505.c:29#2)^guard((d_int32 ==_int32 0));
       -->
        (505.c:29#2)^guard((c_int32 ==_int32 0));
      }
    }
    (505.c:33#4)^x =(int32) 1;
    (505.c:34#4)^x =(int32) 2;
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 d = 0;
int32 x = 0;

