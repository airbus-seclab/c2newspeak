Newspeak output
---------------
f() {
  (068.c:27#1064)^0- =(int32) 1;
}

main() {
  (068.c:31#1097)^int32;
  (068.c:33#1120)^int32;
  (068.c:32#1102)^int32;
  (068.c:32#1102)^do {
    (068.c:32#1102)^while (1) {
      (068.c:32#1102)^{
        int32;
        (068.c:32#1102)^f();
        (068.c:32#1102)^2- =(int32) 0-_int32;
      }
      (068.c:32#1102)^choose {
        | ! (1-_int32 ==_int32 0) -->
        | (1-_int32 ==_int32 0) -->
          (068.c:32#1102)^goto lbl2;
      }
      (068.c:33#1120)^0- =(int32) 2-_int32;
      (068.c:33#1120)^2- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}


