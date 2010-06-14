Newspeak output
---------------
037.c
void main(void) {
  (037.c:30#1132)^int32 y;
  (037.c:29#1112)^do {
    (037.c:31#1137)^choose {
     -->
      (037.c:31#1137)^guard(! (y_int32 ==_int32 2));
      (037.c:31#1137)^guard(! (y_int32 ==_int32 1));
      (037.c:31#1137)^goto lbl0;
     -->
      (037.c:31#1137)^choose {
       -->
        (037.c:31#1137)^guard((y_int32 ==_int32 2));
        (037.c:31#1137)^goto lbl0;
       -->
        (037.c:31#1137)^choose {
         -->
          (037.c:31#1137)^guard((y_int32 ==_int32 1));
          (037.c:31#1137)^goto lbl0;
         -->
        }
      }
    }
  } with lbl0: {
  }
}


