Newspeak output
---------------
110.c
void main(void) {
  int32 tmp;
  (110.c:29#1089)^{
    int32 value_of_f;
    (110.c:29#1089)^f();
    (110.c:29#1089)^1- =(int32) 0-_int32;
  }
  (110.c:29#1089)^choose {
   -->
    (110.c:29#1089)^guard(! (0-_int32 ==_int32 0));
   -->
    (110.c:29#1089)^guard((0-_int32 ==_int32 0));
  }
}


