Newspeak output
---------------
095.c
void main(void) {
  (095.c:27#1072)^int32 x;
  (095.c:26#1052)^do {
    (095.c:28#1077)^while (1) {
      (095.c:28#1077)^choose {
       -->
        (095.c:28#1077)^guard(! (0 > x_int32));
        (095.c:28#1077)^choose {
         -->
          (095.c:28#1077)^guard((10 > x_int32));
         -->
          (095.c:28#1077)^guard(! (10 > x_int32));
          (095.c:28#1077)^goto lbl0;
        }
       -->
        (095.c:28#1077)^guard((0 > x_int32));
        (095.c:28#1077)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


