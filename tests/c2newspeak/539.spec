Newspeak output
---------------
539.c
void main(void) {
  (539.c:29#2)^choose {
   -->
    (539.c:29#2)^guard(! (a_int32 ==_int32 0));
    (539.c:29#2)^choose {
     -->
      (539.c:29#2)^guard(! (b_int32 ==_int32 0));
      (539.c:30#4)^x =(int32) 0;
     -->
      (539.c:29#2)^guard((b_int32 ==_int32 0));
    }
   -->
    (539.c:29#2)^guard((a_int32 ==_int32 0));
    (539.c:29#2)^choose {
     -->
      (539.c:29#2)^guard(! (c_int32 ==_int32 0));
      (539.c:30#4)^x =(int32) 0;
     -->
      (539.c:29#2)^guard((c_int32 ==_int32 0));
    }
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 x = 0;

