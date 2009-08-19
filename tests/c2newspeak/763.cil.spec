Newspeak output
---------------
763.c
void main(void) {
  (763.c:27#1056)^float64 x;
  (763.c:28#1065)^int32 rnd;
  (763.c:29#1072)^choose {
   -->
    (763.c:29#1072)^guard(! (0-_int32 ==_int32 0));
    (763.c:29#1072)^1- =(float64) 1-_float64;
   -->
    (763.c:29#1072)^guard((0-_int32 ==_int32 0));
    (763.c:29#1072)^1- =(float64) (float64 <= int32) -1;
  }
}


