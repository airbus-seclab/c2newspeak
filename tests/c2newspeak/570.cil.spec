Newspeak output
---------------
570.c
void main(void) {
  ptr tmp;
  (570.c:29#1091)^f();
  (570.c:29#1091)^choose {
   -->
    (570.c:29#1091)^guard(! ([0-_ptr]8_int8 ==_int32 0));
   -->
    (570.c:29#1091)^guard(([0-_ptr]8_int8 ==_int32 0));
  }
}


