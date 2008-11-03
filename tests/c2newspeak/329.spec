Newspeak output
---------------
329.c
main() {
  (329.c:29#2)^int32 !tmp-1073741823;
  (329.c:29#2)^do {
    (329.c:29#2)^do {
      (329.c:29#2)^f();
      (329.c:30#2)^do {
        (329.c:29#2)^choose {
          | (0-_int32 ==_int32 1) -->
            (329.c:30#2)^goto lbl4;
          | ! (0-_int32 ==_int32 1) -->
            (329.c:29#2)^goto lbl3;
        }
      } with lbl4: {
      }
      (329.c:31#4)^goto lbl2;
    } with lbl3: {
    }
  } with lbl2: {
  }
}


