Newspeak output
---------------
335.c
void main(void) {
  (335.c:37#1153)^int32 i;
  (335.c:38#1158)^do {
    (335.c:38#1158)^while (1) {
      (335.c:38#1158)^choose {
       -->
        (335.c:38#1158)^guard((10 > i_int32));
       -->
        (335.c:38#1158)^guard(! (10 > i_int32));
        (335.c:38#1158)^goto lbl1;
      }
      (335.c:38#1158)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
    }
  } with lbl1: {
  }
  (335.c:40#1185)^i =(int32) 0;
  (335.c:40#1185)^do {
    (335.c:40#1185)^while (1) {
      (335.c:40#1185)^choose {
       -->
        (335.c:40#1185)^guard((10 > i_int32));
       -->
        (335.c:40#1185)^guard(! (10 > i_int32));
        (335.c:40#1185)^goto lbl2;
      }
    }
  } with lbl2: {
  }
  (335.c:42#1213)^do {
    (335.c:42#1213)^while (1) {
      (335.c:42#1213)^choose {
       -->
        (335.c:42#1213)^guard((10 > i_int32));
       -->
        (335.c:42#1213)^guard(! (10 > i_int32));
        (335.c:42#1213)^goto lbl3;
      }
    }
  } with lbl3: {
  }
  (335.c:44#1236)^while (1) {
  }
}


