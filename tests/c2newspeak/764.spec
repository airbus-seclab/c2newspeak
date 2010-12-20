Warning: 764.c:28#0: comma in expression accepted
Newspeak output
---------------
void main(void) {
  (764.c:27#6)^int32 x;
  (764.c:28#2)^do {
    (764.c:28#2)^while (1) {
      (764.c:28#9)^x =(int32) 1;
      (764.c:28#2)^choose {
       -->
        (764.c:28#2)^guard(x_int32);
       -->
        (764.c:28#2)^guard(! x_int32);
        (764.c:28#2)^goto lbl1;
      }
    }
  } with lbl1:
}


