Newspeak output
---------------
570.c
void main(void) {
  (570.c:29#2)^ptr !tmp-1073741823;
  (570.c:29#2)^f();
  (570.c:29#2)^choose {
   -->
    (570.c:29#2)^guard(! ([0-_ptr]8_int8 ==_int32 0));
   -->
    (570.c:29#2)^guard(([0-_ptr]8_int8 ==_int32 0));
  }
}


