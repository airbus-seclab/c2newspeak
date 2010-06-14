Newspeak output
---------------
764.c
void main(void) {
  (764.c:27#1053)^int32 x;
  (764.c:26#1033)^do {
    (764.c:28#1058)^while (1) {
      (764.c:28#1058)^x =(int32) 1;
      (764.c:28#1058)^choose {
       -->
        (764.c:28#1058)^guard(! (x_int32 ==_int32 0));
       -->
        (764.c:28#1058)^guard((x_int32 ==_int32 0));
        (764.c:28#1058)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


