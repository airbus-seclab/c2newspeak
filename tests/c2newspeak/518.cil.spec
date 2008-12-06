Newspeak output
---------------
518.c
void main(void) {
  (518.c:30#1132)^int32 x;
  (518.c:29#1112)^do {
    (518.c:31#1137)^while (1) {
      (518.c:31#1137)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
          (518.c:31#1137)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


