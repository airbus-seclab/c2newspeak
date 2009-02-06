Newspeak output
---------------
109.c
void main(void) {
  (109.c:27#1072)^int32 x;
  (109.c:28#1077)^choose {
   -->
    (109.c:28#1077)^guard((0-_int32 ==_int32 0));
   -->
    (109.c:28#1077)^guard(! (0-_int32 ==_int32 0));
  }
}


