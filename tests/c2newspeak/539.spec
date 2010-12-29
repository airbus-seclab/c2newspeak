Newspeak output
---------------
void (539.c:28#5)^main(void) {
  (539.c:29#2)^choose {
   -->
    (539.c:29#2)^choose {
     -->
      (539.c:29#2)^guard(! (a_int32 ==_int32 0));
      (539.c:29#2)^guard(! (b_int32 ==_int32 0));
     -->
      (539.c:29#2)^guard((a_int32 ==_int32 0));
      (539.c:29#2)^guard(! (c_int32 ==_int32 0));
    }
    (539.c:30#4)^x =(int32) 0;
   -->
    (539.c:29#2)^choose {
     -->
      (539.c:29#2)^guard(! (a_int32 ==_int32 0));
      (539.c:29#2)^guard((b_int32 ==_int32 0));
     -->
      (539.c:29#2)^guard((a_int32 ==_int32 0));
      (539.c:29#2)^guard((c_int32 ==_int32 0));
    }
  }
}

int32 a;
int32 b;
int32 c;
int32 x;

