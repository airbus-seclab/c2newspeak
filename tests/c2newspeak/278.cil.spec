Newspeak output
---------------
278.c
int32 g(void) {
  (278.c:28#1081)^int32 x;
  (278.c:31#1107)^int32 y;
  (278.c:30#1087)^do {
    (278.c:30#1087)^while (1) {
      (278.c:30#1087)^choose {
       -->
        (278.c:30#1087)^guard(! (x_int32 ==_int32 0));
       -->
        (278.c:30#1087)^guard((x_int32 ==_int32 0));
        (278.c:30#1087)^goto lbl1;
      }
      (278.c:32#1114)^goto lbl1;
      (278.c:33#1125)^y =(int32) 1;
    }
  } with lbl1: {
  }
  (278.c:36#1139)^!return =(int32) x_int32;
}


