Newspeak output
---------------
void (112.c:26#5)^main(void) {
  (112.c:27#6)^int32 x;
  (112.c:28#2)^choose {
   -->
    (112.c:28#2)^guard((x_int32 ==_int32 0));
   -->
    (112.c:28#2)^guard(! (x_int32 ==_int32 0));
  }
}


