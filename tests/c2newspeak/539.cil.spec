Newspeak output
---------------
539.c
void main(void) {
  int32 tmp;
  (539.c:29#1088)^choose {
   -->
    (539.c:29#1088)^guard(! (a_int32 ==_int32 0));
    (539.c:29#1088)^tmp =(int32) b_int32;
   -->
    (539.c:29#1088)^guard((a_int32 ==_int32 0));
    (539.c:29#1088)^tmp =(int32) c_int32;
  }
  (539.c:29#1088)^choose {
   -->
    (539.c:29#1088)^guard(! (tmp_int32 ==_int32 0));
    (539.c:30#1105)^x =(int32) 0;
   -->
    (539.c:29#1088)^guard((tmp_int32 ==_int32 0));
  }
}

int32 a;
int32 b;
int32 c;
int32 x;

