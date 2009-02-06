Newspeak output
---------------
433.c
void main(void) {
  (433.c:27#1072)^int32 x;
  (433.c:28#1077)^choose {
   -->
    (433.c:28#1077)^guard(! (0-_int32 ==_int32 0));
   -->
    (433.c:28#1077)^guard((0-_int32 ==_int32 0));
  }
}


