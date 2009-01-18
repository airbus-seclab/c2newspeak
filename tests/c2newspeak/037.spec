Newspeak output
---------------
037.c
void main(void) {
  (037.c:30#6)^int32 y;
  (037.c:31#2)^do {
    (037.c:31#2)^choose {
     -->
      (037.c:31#2)^guard((0-_int32 ==_int32 2));
      (037.c:32#2)^goto lbl1;
     -->
      (037.c:31#2)^guard((0-_int32 ==_int32 1));
      (037.c:35#2)^goto lbl1;
     -->
      (037.c:31#2)^guard(! (0-_int32 ==_int32 1));
      (037.c:31#2)^guard(! (0-_int32 ==_int32 2));
      (037.c:31#2)^goto lbl1;
    }
  } with lbl1: {
  }
}


