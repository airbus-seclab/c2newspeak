Warning: 433.c:28#2: cast to void accepted
Warning: 433.c:28#2: cast to void accepted
Newspeak output
---------------
433.c
void main(void) {
  (433.c:27#6)^int32 x;
  (433.c:28#2)^choose {
   -->
    (433.c:28#2)^guard(! (0-_int32 ==_int32 0));
   -->
    (433.c:28#2)^guard((0-_int32 ==_int32 0));
  }
}


