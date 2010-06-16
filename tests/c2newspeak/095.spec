Newspeak output
---------------
095.c
void main(void) {
  (095.c:27#6)^int32 x;
  (095.c:28#2)^do {
    (095.c:28#2)^while (1) {
      (095.c:28#2)^choose {
       -->
        (095.c:28#2)^guard(! (0 > x_int32));
        (095.c:28#2)^guard((10 > x_int32));
       -->
        (095.c:28#2)^choose {
         -->
          (095.c:28#2)^guard(! (0 > x_int32));
          (095.c:28#2)^guard(! (10 > x_int32));
         -->
          (095.c:28#2)^guard((0 > x_int32));
        }
        (095.c:28#2)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}


