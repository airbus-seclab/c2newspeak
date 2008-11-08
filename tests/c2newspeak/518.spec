Newspeak output
---------------
518.c
main() {
  (518.c:27#6)^int32 x;
  (518.c:28#2)^do {
    (518.c:29#2)^do {
      (518.c:28#2)^choose {
        | (0-_int32 ==_int32 2) -->
          (518.c:29#2)^goto lbl4;
        | ! (0-_int32 ==_int32 2) -->
          (518.c:28#2)^goto lbl3;
      }
    } with lbl4: {
    }
    (518.c:30#4)^0- =(int32) 1;
  } with lbl3: {
  }
}


