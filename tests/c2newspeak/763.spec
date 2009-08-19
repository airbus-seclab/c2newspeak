Newspeak output
---------------
763.c
void main(void) {
  (763.c:27#9)^float64 x;
  (763.c:28#6)^int32 rnd;
  (763.c:29#2)^choose {
   -->
    (763.c:29#2)^guard(! (0-_int32 ==_int32 0));
    (763.c:29#2)^1- =(float64) 1-_float64;
   -->
    (763.c:29#2)^guard((0-_int32 ==_int32 0));
    (763.c:29#2)^1- =(float64) (float64 <= int32) -1;
  }
}


