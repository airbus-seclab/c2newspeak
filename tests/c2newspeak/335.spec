Warning: 335.c:27#1067: identifier s is defined as a type, avoid using it for another purpose
Warning: 335.c:32#1102: 'short' is not normalized: use 'short int' instead
Warning: 335.c:33#1111: 'long' is not normalized: use 'long int' instead
Warning: 335.c:34#1119: 'long long' is not standard: use 'long long int' instead
Warning: 335.c:40#1183: init statement expected
Warning: 335.c:42#1211: increment statement expected
Warning: 335.c:44#1234: init statement expected
Warning: 335.c:44#1234: halting condition should be explicit
Warning: 335.c:46#1251: init statement expected
Newspeak output
---------------
335.c
void main(void) {
  (335.c:37#6)^int32 i;
  (335.c:38#2)^do {
    (335.c:38#2)^while (1) {
      (335.c:38#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:38#2)^goto lbl1;
      }
      (335.c:38#16)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
  (335.c:40#6)^0- =(int32) 0;
  (335.c:40#2)^do {
    (335.c:40#2)^while (1) {
      (335.c:40#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:40#2)^goto lbl4;
      }
    }
  } with lbl4: {
  }
  (335.c:42#2)^do {
    (335.c:42#2)^while (1) {
      (335.c:42#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (335.c:42#2)^goto lbl7;
      }
    }
  } with lbl7: {
  }
  (335.c:44#2)^while (1) {
  }
}


