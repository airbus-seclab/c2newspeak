Newspeak output
---------------
519.c
void main(void) {
  (519.c:30#6)^int32 y;
  (519.c:29#6)^int32 x;
  (519.c:28#5)^do {
    (519.c:31#2)^choose {
     -->
      (519.c:31#2)^guard(! (x_int32 ==_int32 0));
      (519.c:31#9)^goto lbl0;
     -->
      (519.c:31#2)^guard((x_int32 ==_int32 0));
    }
    (519.c:32#2)^y <- f();
  } with lbl0:
}


