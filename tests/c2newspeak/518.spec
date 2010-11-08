Newspeak output
---------------
518.c
void main(void) {
  (518.c:30#6)^int32 x;
  (518.c:31#2)^do {
    (518.c:31#2)^while (1) {
      (518.c:31#2)^choose {
       -->
        (518.c:31#2)^guard(! (x_int32 ==_int32 0));
       -->
        (518.c:31#2)^guard((x_int32 ==_int32 0));
        (518.c:31#2)^goto lbl1;
      }
    }
  } with lbl1:
}


