Newspeak output
---------------
main() {
  (047.c:30#1132)^int32;
  (047.c:32#1140)^0- =(int32) 0;
  (047.c:32#1140)^do {
    (047.c:32#1140)^while (1) {
      (047.c:32#1140)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (047.c:32#1140)^goto lbl2;
      }
      (047.c:32#1140)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}


