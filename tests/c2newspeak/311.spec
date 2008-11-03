Warning: init statement expected in 311.c line 31
Newspeak output
---------------
311.c
main() {
  (311.c:27#6)^int32 i;
  (311.c:28#2)^0- =(int32) 0;
  (311.c:29#2)^do {
    (311.c:29#2)^while (1) {
      (311.c:29#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (311.c:29#2)^goto lbl2;
      }
      (311.c:29#18)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}


