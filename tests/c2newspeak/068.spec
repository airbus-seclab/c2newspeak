Newspeak output
---------------
068.c
f() {
  (068.c:27#2)^0- =(int32) 1;
}

main() {
  (068.c:31#6)^int32 x;
  (068.c:32#2)^int32 !tmp-1073741821;
  (068.c:32#2)^do {
    (068.c:32#2)^while (1) {
      (068.c:32#2)^f();
      (068.c:32#2)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
          (068.c:32#2)^goto lbl2;
      }
      (068.c:33#4)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
    }
  } with lbl2: {
  }
}


