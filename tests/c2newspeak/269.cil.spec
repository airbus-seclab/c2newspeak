Newspeak output
---------------
269.c
void main(void) {
  (269.c:27#1072)^int32 x;
  (269.c:26#1052)^do {
    (269.c:28#1077)^while (1) {
      (269.c:28#1077)^0- =(int32) 0;
      (269.c:28#1077)^choose {
       -->
        (269.c:28#1077)^guard(! (0-_int32 ==_int32 0));
       -->
        (269.c:28#1077)^guard((0-_int32 ==_int32 0));
        (269.c:28#1077)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


