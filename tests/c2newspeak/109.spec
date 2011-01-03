Newspeak output
---------------
void (109.c:26#5)^main(void) {
  (109.c:27#6)^int32 x;
  (109.c:28#2)^choose {
   -->
    (109.c:28#2)^guard((x_int32 ==_int32 0));
   -->
    (109.c:28#2)^guard(! (x_int32 ==_int32 0));
  }
}


