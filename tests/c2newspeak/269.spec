Warning: 269.c:28#2: assignment within expression accepted
Newspeak output
---------------
void main(void) {
  (269.c:27#6)^int32 x;
  (269.c:28#2)^do {
    (269.c:28#2)^while (1) {
      (269.c:28#2)^x =(int32) 0;
      (269.c:28#2)^choose {
       -->
        (269.c:28#2)^guard(! (x_int32 ==_int32 0));
       -->
        (269.c:28#2)^guard((x_int32 ==_int32 0));
        (269.c:28#2)^goto lbl1;
      }
    }
  } with lbl1:
}


