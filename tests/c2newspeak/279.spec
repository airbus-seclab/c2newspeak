Newspeak output
---------------
279.c
g() {
  (279.c:28#6)^int32 x;
  (279.c:30#2)^{
    int32 !tmp-1073741822;
    (279.c:30#2)^do {
      (279.c:30#2)^while (1) {
        (279.c:30#2)^f();
        (279.c:30#2)^choose {
          | ! (0-_int32 ==_int32 0) -->
          | (0-_int32 ==_int32 0) -->
            (279.c:30#2)^goto lbl2;
        }
      }
    } with lbl2: {
    }
  }
  (279.c:33#2)^1- =(int32) 0-_int32;
}


