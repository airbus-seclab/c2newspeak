Newspeak output
---------------
047.c
main() {
  (047.c:30#6)^int32 x;
  (047.c:32#7)^0- =(int32) 0;
  (047.c:32#2)^do {
    (047.c:32#2)^while (1) {
      (047.c:32#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (047.c:32#2)^goto lbl1;
      }
      (047.c:32#22)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
}


