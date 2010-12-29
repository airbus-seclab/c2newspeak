Newspeak output
---------------
void (037.c:29#5)^main(void) {
  (037.c:30#6)^int32 y;
  (037.c:31#2)^do {
    (037.c:31#2)^choose {
     -->
      (037.c:31#2)^guard((y_int32 ==_int32 2));
      (037.c:32#2)^goto lbl1;
     -->
      (037.c:31#2)^choose {
       -->
        (037.c:31#2)^guard((y_int32 ==_int32 1));
        (037.c:35#2)^goto lbl1;
       -->
        (037.c:31#2)^guard(! (y_int32 ==_int32 1));
        (037.c:31#2)^guard(! (y_int32 ==_int32 2));
        (037.c:31#2)^goto lbl1;
      }
    }
  } with lbl1:
}


