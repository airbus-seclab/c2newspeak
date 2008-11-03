Newspeak output
---------------
020.c
main() {
  (020.c:31#6)^int32 x;
  (020.c:32#2)^do {
    (020.c:34#2)^do {
      (020.c:32#2)^choose {
        | (0-_int32 ==_int32 1) -->
          (020.c:33#2)^goto lbl4;
        | (0-_int32 ==_int32 2) -->
          (020.c:34#2)^goto lbl4;
        | ! (0-_int32 ==_int32 2) & ! (0-_int32 ==_int32 1) -->
          (020.c:32#2)^goto lbl3;
      }
    } with lbl4: {
    }
    (020.c:35#4)^0- =(int32) 1;
  } with lbl3: {
  }
}


