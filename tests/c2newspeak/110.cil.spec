Newspeak output
---------------
110.c
void main(void) {
  int32 tmp;
  (110.c:29#1089)^tmp <- f();
  (110.c:29#1089)^choose {
   -->
    (110.c:29#1089)^guard(! (tmp_int32 ==_int32 0));
   -->
    (110.c:29#1089)^guard((tmp_int32 ==_int32 0));
  }
}


