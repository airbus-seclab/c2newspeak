Newspeak output
---------------
199.c
void main(void) {
  (199.c:27#1073)^int32 x;
  (199.c:26#1052)^do {
    (199.c:28#1078)^while (1) {
      (199.c:29#1087)^choose {
        | ! (0-_int32 ==_int32 0) -->
          (199.c:30#1102)^goto lbl0;
        | (0-_int32 ==_int32 0) -->
      }
      (199.c:28#1078)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (199.c:28#1078)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


