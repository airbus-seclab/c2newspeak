Newspeak output
---------------
278.c
g() {
  (278.c:28#1081)^int32;
  (278.c:31#1107)^int32;
  (278.c:30#1087)^do {
    (278.c:30#1087)^while (1) {
      (278.c:30#1087)^choose {
        | ! (1-_int32 ==_int32 0) -->
        | (1-_int32 ==_int32 0) -->
          (278.c:30#1087)^goto lbl2;
      }
      (278.c:32#1114)^goto lbl2;
      (278.c:33#1125)^0- =(int32) 1;
    }
  } with lbl2: {
  }
  (278.c:36#1139)^2- =(int32) 1-_int32;
}


