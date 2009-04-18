Newspeak output
---------------
045.c
void main(void) {
  (045.c:30#6)^int32 x;
  (045.c:32#2)^choose {
   -->
    (045.c:32#2)^guard((0-_int32 > 0));
    (045.c:32#2)^guard((5 > 0-_int32));
   -->
    (045.c:32#2)^choose {
     -->
      (045.c:32#2)^guard((0-_int32 > 0));
      (045.c:32#2)^guard(! (5 > 0-_int32));
     -->
      (045.c:32#2)^guard(! (0-_int32 > 0));
    }
  }
}


