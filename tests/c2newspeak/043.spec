Newspeak output
---------------
043.c
void main(void) {
  (043.c:30#6)^int32 x;
  (043.c:31#2)^choose {
   -->
    (043.c:31#2)^guard((0-_int32 > 0));
   -->
    (043.c:31#2)^guard(! (0-_int32 > 0));
  }
}


