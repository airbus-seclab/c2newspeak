Newspeak output
---------------
068.c
f() {
  (068.c:27#1064)^0- =(int32) 1;
}

main() {
  (068.c:31#1097)^int32 x;
  int32 tmp;
  (068.c:30#1077)^do {
    (068.c:32#1102)^while (1) {
      (068.c:32#1102)^{
        int32 value_of_f;
        (068.c:32#1102)^f();
        (068.c:32#1102)^1- =(int32) 0-_int32;
      }
      (068.c:32#1102)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
          (068.c:32#1102)^goto lbl0;
      }
      (068.c:33#1120)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
    }
  } with lbl0: {
  }
}


