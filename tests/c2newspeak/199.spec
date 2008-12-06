Newspeak output
---------------
199.c
void main(void) {
  (199.c:27#6)^int32 x;
  (199.c:28#2)^do {
    (199.c:29#4)^choose {
      | ! (0-_int32 ==_int32 0) -->
        (199.c:30#6)^goto lbl1;
      | (0-_int32 ==_int32 0) -->
    }
    (199.c:28#2)^while (1) {
      (199.c:28#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (199.c:28#2)^goto lbl1;
      }
      (199.c:29#4)^choose {
        | ! (0-_int32 ==_int32 0) -->
          (199.c:30#6)^goto lbl1;
        | (0-_int32 ==_int32 0) -->
      }
    }
  } with lbl1: {
  }
}


