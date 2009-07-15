Newspeak output
---------------
508.c
void main(void) {
  (508.c:29#2)^choose {
   -->
    (508.c:29#2)^choose {
     -->
      (508.c:29#2)^guard(! (a_int32 ==_int32 0));
      (508.c:29#2)^choose {
       -->
        (508.c:29#2)^guard(! (b_int32 ==_int32 0));
       -->
        (508.c:29#2)^guard((b_int32 ==_int32 0));
        (508.c:29#2)^guard(! (c_int32 ==_int32 0));
        (508.c:29#2)^guard(! (d_int32 ==_int32 0));
      }
     -->
      (508.c:29#2)^guard((a_int32 ==_int32 0));
      (508.c:29#2)^guard(! (c_int32 ==_int32 0));
      (508.c:29#2)^guard(! (d_int32 ==_int32 0));
    }
    (508.c:30#4)^x =(int32) -2;
    (508.c:31#4)^x =(int32) -1;
    (508.c:32#4)^x =(int32) 0;
    (508.c:33#4)^x =(int32) 3;
   -->
    (508.c:29#2)^choose {
     -->
      (508.c:29#2)^guard(! (a_int32 ==_int32 0));
      (508.c:29#2)^guard((b_int32 ==_int32 0));
      (508.c:29#2)^choose {
       -->
        (508.c:29#2)^guard(! (c_int32 ==_int32 0));
        (508.c:29#2)^guard((d_int32 ==_int32 0));
       -->
        (508.c:29#2)^guard((c_int32 ==_int32 0));
      }
     -->
      (508.c:29#2)^guard((a_int32 ==_int32 0));
      (508.c:29#2)^choose {
       -->
        (508.c:29#2)^guard(! (c_int32 ==_int32 0));
        (508.c:29#2)^guard((d_int32 ==_int32 0));
       -->
        (508.c:29#2)^guard((c_int32 ==_int32 0));
      }
    }
    (508.c:35#4)^x =(int32) -2;
    (508.c:36#4)^x =(int32) -1;
    (508.c:37#4)^x =(int32) 1;
    (508.c:38#4)^x =(int32) 2;
  }
}

int32 a;
int32 b;
int32 c;
int32 d;
int32 x;

