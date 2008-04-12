Warning: labels and goto statements are error-prone, they should be avoided at all costs in 332.c line 32
Warning: labels and goto statements are error-prone, they should be avoided at all costs in 332.c line 34
Newspeak output
---------------
332.c
main() {
  (332.c:30#1132)^int32;
  (332.c:35#1174)^do {
    (332.c:31#1137)^choose {
      | ! (0-_int32 ==_int32 0) -->
        (332.c:32#1150)^goto lbl4;
      | (0-_int32 ==_int32 0) -->
    }
    (332.c:34#1166)^0- =(int32) 3;
  } with lbl4: {
  }
  (332.c:36#1181)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
}


