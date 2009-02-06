Newspeak output
---------------
539.c
void main(void) {
  int32 tmp;
  (539.c:29#1088)^choose {
   -->
    (539.c:29#1088)^guard(! (a_int32 ==_int32 0));
    (539.c:29#1088)^0- =(int32) b_int32;
   -->
    (539.c:29#1088)^guard((a_int32 ==_int32 0));
    (539.c:29#1088)^0- =(int32) c_int32;
  }
  (539.c:29#1088)^choose {
   -->
    (539.c:29#1088)^guard(! (0-_int32 ==_int32 0));
    (539.c:30#1105)^x =(int32) 0;
   -->
    (539.c:29#1088)^guard((0-_int32 ==_int32 0));
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 x = 0;

