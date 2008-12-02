Warning: 555.c:33#1137: switch with default case in intermediary position accepted
Newspeak output
---------------
555.c
main() {
  (555.c:27#5)^int32 i;
  (555.c:28#1)^do {
    (555.c:28#1)^choose {
      | (0-_int32 ==_int32 0) -->
        (555.c:31#3)^goto lbl1;
      | ! (0-_int32 ==_int32 0) -->
        (555.c:28#1)^goto lbl1;
    }
  } with lbl1: {
  }
}


