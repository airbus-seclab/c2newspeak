Newspeak output
---------------
277.c
void main(void) {
  (277.c:28#1085)^int32 x;
  int32 tmp;
  (277.c:27#1065)^do {
    (277.c:31#1106)^do {
      (277.c:30#1091)^choose {
       -->
        (277.c:30#1091)^guard(! (1-_int32 ==_int32 0));
        (277.c:30#1091)^goto lbl0;
       -->
        (277.c:30#1091)^guard((1-_int32 ==_int32 0));
        (277.c:30#1091)^goto lbl1;
      }
    } with lbl1: {
    }
    (277.c:32#1118)^{
      int32 value_of_g;
      (277.c:32#1118)^g();
      (277.c:32#1118)^1- =(int32) 0-_int32;
    }
    (277.c:32#1118)^choose {
     -->
      (277.c:32#1118)^guard(! (0-_int32 ==_int32 0));
     -->
      (277.c:32#1118)^guard((0-_int32 ==_int32 0));
    }
  } with lbl0: {
  }
}


