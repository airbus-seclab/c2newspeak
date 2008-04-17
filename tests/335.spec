Warning: identifier s is defined as a type, avoid using it for another purpose in 335.c line 27
Warning: 'short' is not normalized: use 'short int' instead in 335.c line 32
Warning: 'long' is not normalized: use 'long int' instead in 335.c line 33
Warning: 'long long' is not standard: use 'long long int' instead in 335.c line 34
Warning: init statement expected in 335.c line 39
Warning: increment statement expected in 335.c line 41
Warning: init statement expected in 335.c line 43
Warning: halting condition should be explicit in 335.c line 44
Warning: init statement expected in 335.c line 45
Newspeak output
---------------
335.c
main() {
  (335.c:37#1153)^int32;
  (335.c:38#1158)^do {
    (335.c:38#1158)^while (1) {
      (335.c:38#1158)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:38#1158)^goto lbl2;
      }
      (335.c:38#1172)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
  (335.c:40#1189)^0- =(int32) 0;
  (335.c:40#1185)^do {
    (335.c:40#1185)^while (1) {
      (335.c:40#1185)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:40#1185)^goto lbl2;
      }
    }
  } with lbl2: {
  }
  (335.c:42#1213)^do {
    (335.c:42#1213)^while (1) {
      (335.c:42#1213)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:42#1213)^goto lbl2;
      }
    }
  } with lbl2: {
  }
  (335.c:44#1236)^while (1) {
  }
}


