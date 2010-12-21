Warning: 433.c:28#2: cast to void accepted
Warning: 433.c:28#2: cast to void accepted
Newspeak output
---------------
void main(void) {
  (433.c:27#6)^int32 x;
  (433.c:28#2)^choose {
   -->
    (433.c:28#2)^guard(! (x_int32 ==_int32 0));
   -->
    (433.c:28#2)^guard((x_int32 ==_int32 0));
  }
}


