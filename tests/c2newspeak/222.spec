Newspeak output
---------------
void (222.c:26#5)^main(void) {
  (222.c:27#6)^int32 x;
  (222.c:28#2)^do {
    (222.c:28#2)^while (1) {
      (222.c:28#2)^choose {
       -->
        (222.c:28#2)^guard(! (0 > x_int32));
       -->
        (222.c:28#2)^guard((0 > x_int32));
        (222.c:28#2)^goto lbl1;
      }
    }
  } with lbl1:
}


