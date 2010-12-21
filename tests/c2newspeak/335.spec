Warning: 335.c:27#0: identifier s is defined as a type, avoid using it for another purpose
Warning: 335.c:32#0: 'short' is not normalized: use 'short int' instead
Warning: 335.c:33#0: 'long' is not normalized: use 'long int' instead
Warning: 335.c:34#0: 'long long' is not standard: use 'long long int' instead
Warning: 335.c:39#0: init statement expected
Warning: 335.c:41#0: increment statement expected
Warning: 335.c:43#0: init statement expected
Warning: 335.c:44#0: halting condition should be explicit
Warning: 335.c:45#0: init statement expected
Newspeak output
---------------
void main(void) {
  (335.c:37#6)^int32 i;
  (335.c:38#2)^do {
    (335.c:38#2)^while (1) {
      (335.c:38#2)^choose {
       -->
        (335.c:38#2)^guard((10 > i_int32));
       -->
        (335.c:38#2)^guard(! (10 > i_int32));
        (335.c:38#2)^goto lbl1;
      }
      (335.c:38#16)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
    }
  } with lbl1:
  (335.c:40#6)^i =(int32) 0;
  (335.c:40#2)^do {
    (335.c:40#2)^while (1) {
      (335.c:40#2)^choose {
       -->
        (335.c:40#2)^guard((10 > i_int32));
       -->
        (335.c:40#2)^guard(! (10 > i_int32));
        (335.c:40#2)^goto lbl4;
      }
    }
  } with lbl4:
  (335.c:42#2)^do {
    (335.c:42#2)^while (1) {
      (335.c:42#2)^choose {
       -->
        (335.c:42#2)^guard((10 > i_int32));
       -->
        (335.c:42#2)^guard(! (10 > i_int32));
        (335.c:42#2)^goto lbl7;
      }
    }
  } with lbl7:
  (335.c:44#2)^while (1) {
  }
}


