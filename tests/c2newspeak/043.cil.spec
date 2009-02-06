Newspeak output
---------------
043.c
void main(void) {
  (043.c:30#1132)^int32 x;
  (043.c:31#1137)^choose {
   -->
    (043.c:31#1137)^guard((0-_int32 > 0));
   -->
    (043.c:31#1137)^guard(! (0-_int32 > 0));
  }
}


