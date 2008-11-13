Newspeak output
---------------
519.c
main() {
  (519.c:30#6)^int32 y;
  (519.c:29#6)^int32 x;
  (519.c:28#5)^do {
    (519.c:31#2)^choose {
      | ! (0-_int32 ==_int32 0) -->
        (519.c:31#9)^goto lbl0;
      | (0-_int32 ==_int32 0) -->
    }
    (519.c:32#2)^{
      int32 value_of_f;
      (519.c:32#2)^f();
      (519.c:32#2)^2- =(int32) 0-_int32;
    }
  } with lbl0: {
  }
}


