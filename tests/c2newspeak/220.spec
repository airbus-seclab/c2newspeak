Newspeak output
---------------
220.c
main() {
  (220.c:31#2)^int32 !tmp-1073741822;
  (220.c:29#6)^int32 x;
  (220.c:28#5)^do {
    (220.c:30#2)^choose {
      | ! (0-_int32 ==_int32 0) -->
        (220.c:30#9)^goto lbl0;
      | (0-_int32 ==_int32 0) -->
    }
    (220.c:31#2)^{
      int32 value_of_f;
      (220.c:31#2)^f();
      (220.c:31#2)^2- =(int32) 0-_int32;
    }
    (220.c:31#2)^choose {
      | ! (1-_int32 ==_int32 0) -->
        (220.c:31#11)^goto lbl0;
      | (1-_int32 ==_int32 0) -->
    }
  } with lbl0: {
  }
}


