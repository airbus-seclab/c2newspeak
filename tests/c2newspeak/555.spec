Warning: 555.c:33#0: switch with default case in intermediary position accepted
Newspeak output
---------------
555.c
void main(void) {
  (555.c:27#5)^int32 i;
  (555.c:28#1)^do {
    (555.c:28#1)^choose {
     -->
      (555.c:28#1)^guard((i_int32 ==_int32 0));
      (555.c:31#3)^goto lbl1;
     -->
      (555.c:28#1)^guard(! (i_int32 ==_int32 0));
      (555.c:28#1)^goto lbl1;
    }
  } with lbl1:
}


