Newspeak output
---------------
void (045.c:29#5)^main(void) {
  (045.c:30#6)^int32 x;
  (045.c:32#2)^choose {
   -->
    (045.c:32#2)^guard((x_int32 > 0));
    (045.c:32#2)^guard((5 > x_int32));
   -->
    (045.c:32#2)^choose {
     -->
      (045.c:32#2)^guard((x_int32 > 0));
      (045.c:32#2)^guard(! (5 > x_int32));
     -->
      (045.c:32#2)^guard(! (x_int32 > 0));
    }
  }
}


