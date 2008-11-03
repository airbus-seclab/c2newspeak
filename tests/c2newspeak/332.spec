Warning: goto statements are error-prone, they should be avoided at all costs in 332.c line 29
Newspeak output
---------------
332.c
main() {
  (332.c:27#6)^int32 x;
  (332.c:32#1)^do {
    (332.c:28#2)^choose {
      | ! (0-_int32 ==_int32 0) -->
        (332.c:29#4)^goto lbl4;
      | (0-_int32 ==_int32 0) -->
    }
    (332.c:31#2)^0- =(int32) 3;
  } with lbl4: {
  }
  (332.c:33#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
}


