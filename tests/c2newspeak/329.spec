Newspeak output
---------------
329.c
void main(void) {
  (329.c:29#2)^int32 !tmp-1073741823;
  (329.c:29#2)^f();
  (329.c:29#2)^do {
    (329.c:29#2)^choose {
      | (0-_int32 ==_int32 1) -->
        (329.c:30#2)^goto lbl1;
      | ! (0-_int32 ==_int32 1) -->
        (329.c:29#2)^goto lbl1;
    }
  } with lbl1: {
  }
}


