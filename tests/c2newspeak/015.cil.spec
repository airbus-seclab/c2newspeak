Newspeak output
---------------
015.c
main() {
  (015.c:30#1132)^int32 i;
  (015.c:30#1128)^0- =(int32) 0;
  (015.c:32#1144)^do {
    (015.c:33#1153)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    (015.c:32#1144)^while (1) {
      (015.c:32#1144)^choose {
        | (100 > 0-_int32) -->
        | ! (100 > 0-_int32) -->
          (015.c:32#1144)^goto lbl2;
      }
      (015.c:33#1153)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}

