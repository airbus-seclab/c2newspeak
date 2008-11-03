Warning: identifier s is defined as a type, avoid using it for another purpose in 335.c line 27
Warning: 'short' is not normalized: use 'short int' instead in 335.c line 32
Warning: 'long' is not normalized: use 'long int' instead in 335.c line 33
Warning: 'long long' is not standard: use 'long long int' instead in 335.c line 34
Warning: init statement expected in 335.c line 40
Warning: increment statement expected in 335.c line 42
Warning: init statement expected in 335.c line 44
Warning: halting condition should be explicit in 335.c line 44
Warning: init statement expected in 335.c line 46
Newspeak output
---------------
335.c
main() {
  (335.c:37#6)^int32 i;
  (335.c:38#2)^do {
    (335.c:38#2)^while (1) {
      (335.c:38#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:38#2)^goto lbl2;
      }
      (335.c:38#16)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
  (335.c:40#6)^0- =(int32) 0;
  (335.c:40#2)^do {
    (335.c:40#2)^while (1) {
      (335.c:40#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:40#2)^goto lbl2;
      }
    }
  } with lbl2: {
  }
  (335.c:42#2)^do {
    (335.c:42#2)^while (1) {
      (335.c:42#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:42#2)^goto lbl2;
      }
    }
  } with lbl2: {
  }
  (335.c:44#2)^while (1) {
  }
}


