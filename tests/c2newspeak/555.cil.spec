Newspeak output
---------------
555.c
void main(void) {
  (555.c:27#1074)^int32 i;
  (555.c:26#1052)^do {
    (555.c:28#1078)^choose {
     -->
      (555.c:28#1078)^guard(! (i_int32 ==_int32 0));
      (555.c:28#1078)^goto lbl0;
     -->
      (555.c:28#1078)^choose {
       -->
        (555.c:28#1078)^guard((i_int32 ==_int32 0));
        (555.c:28#1078)^goto lbl0;
       -->
      }
    }
  } with lbl0: {
  }
}


