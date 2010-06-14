Newspeak output
---------------
045.c
void main(void) {
  (045.c:30#1132)^int32 x;
  (045.c:32#1140)^choose {
   -->
    (045.c:32#1140)^guard((x_int32 > 0));
    (045.c:32#1140)^choose {
     -->
      (045.c:32#1140)^guard((5 > x_int32));
     -->
      (045.c:32#1140)^guard(! (5 > x_int32));
    }
   -->
    (045.c:32#1140)^guard(! (x_int32 > 0));
  }
}


