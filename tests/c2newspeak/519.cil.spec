Newspeak output
---------------
519.c
void main(void) {
  (519.c:29#1093)^int32 x;
  (519.c:30#1102)^int32 y;
  (519.c:28#1073)^do {
    (519.c:31#1107)^choose {
     -->
      (519.c:31#1107)^guard(! (1-_int32 ==_int32 0));
      (519.c:31#1114)^goto lbl0;
     -->
      (519.c:31#1107)^guard((1-_int32 ==_int32 0));
    }
    (519.c:32#1124)^f();
  } with lbl0: {
  }
}


