Newspeak output
---------------
void (507.c:28#5)^main(void) {
  (507.c:29#2)^choose {
   -->
    (507.c:29#2)^choose {
     -->
      (507.c:29#2)^guard(! (a_int32 ==_int32 0));
      (507.c:29#2)^choose {
       -->
        (507.c:29#2)^guard(! (b_int32 ==_int32 0));
       -->
        (507.c:29#2)^guard((b_int32 ==_int32 0));
        (507.c:29#2)^guard(! (c_int32 ==_int32 0));
        (507.c:29#2)^guard(! (d_int32 ==_int32 0));
      }
     -->
      (507.c:29#2)^guard((a_int32 ==_int32 0));
      (507.c:29#2)^guard(! (c_int32 ==_int32 0));
      (507.c:29#2)^guard(! (d_int32 ==_int32 0));
    }
    (507.c:30#4)^x =(int32) -1;
    (507.c:31#4)^x =(int32) 0;
    (507.c:32#4)^x =(int32) 3;
   -->
    (507.c:29#2)^choose {
     -->
      (507.c:29#2)^guard(! (a_int32 ==_int32 0));
      (507.c:29#2)^guard((b_int32 ==_int32 0));
      (507.c:29#2)^choose {
       -->
        (507.c:29#2)^guard(! (c_int32 ==_int32 0));
        (507.c:29#2)^guard((d_int32 ==_int32 0));
       -->
        (507.c:29#2)^guard((c_int32 ==_int32 0));
      }
     -->
      (507.c:29#2)^guard((a_int32 ==_int32 0));
      (507.c:29#2)^choose {
       -->
        (507.c:29#2)^guard(! (c_int32 ==_int32 0));
        (507.c:29#2)^guard((d_int32 ==_int32 0));
       -->
        (507.c:29#2)^guard((c_int32 ==_int32 0));
      }
    }
    (507.c:34#4)^x =(int32) -1;
    (507.c:35#4)^x =(int32) 1;
    (507.c:36#4)^x =(int32) 2;
  }
}

int32 a;
int32 b;
int32 c;
int32 d;
int32 x;

