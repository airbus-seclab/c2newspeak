Newspeak output
---------------
674.c
void main(void) {
  (674.c:30#1064)^choose {
   -->
    (674.c:30#1064)^guard(! (a_int32 ==_int32 0));
    (674.c:30#1064)^choose {
     -->
      (674.c:30#1064)^guard(! (b_int32 ==_int32 0));
      (674.c:31#1082)^c =(int32) 1;
     -->
      (674.c:30#1064)^guard((b_int32 ==_int32 0));
    }
   -->
    (674.c:30#1064)^guard((a_int32 ==_int32 0));
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;

