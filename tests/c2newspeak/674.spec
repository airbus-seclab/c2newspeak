Newspeak output
---------------
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

int32 a;
int32 b;
int32 c;

