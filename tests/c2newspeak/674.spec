Newspeak output
---------------
674.c
void main(void) {
  (674.c:30#2)^choose {
   -->
    (674.c:30#2)^guard(! (a_int32 ==_int32 0));
    (674.c:30#2)^guard(! (b_int32 ==_int32 0));
    (674.c:31#4)^c =(int32) 1;
   -->
    (674.c:30#2)^choose {
     -->
      (674.c:30#2)^guard(! (a_int32 ==_int32 0));
      (674.c:30#2)^guard((b_int32 ==_int32 0));
     -->
      (674.c:30#2)^guard((a_int32 ==_int32 0));
    }
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
