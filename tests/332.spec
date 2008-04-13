Warning: labels and goto statements are error-prone, they should be avoided at all costs in 332.c line 29
Warning: labels and goto statements are error-prone, they should be avoided at all costs in 332.c line 31
Newspeak output
---------------
332.c
main() {
  (332.c:27#1072)^int32;
  (332.c:32#1114)^do {
    (332.c:28#1077)^choose {
      | ! (0-_int32 ==_int32 0) -->
        (332.c:29#1090)^goto lbl4;
      | (0-_int32 ==_int32 0) -->
    }
    (332.c:31#1106)^0- =(int32) 3;
  } with lbl4: {
  }
  (332.c:33#1121)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
}


