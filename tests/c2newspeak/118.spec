Newspeak output
---------------
118.c
void main(void) {
  (118.c:27#6)^int32 x;
  (118.c:28#6)^int32 y;
  (118.c:29#2)^choose {
   -->
    (118.c:29#2)^guard(! (1-_int32 ==_int32 0));
   -->
    (118.c:29#2)^guard((1-_int32 ==_int32 0));
    (118.c:29#2)^choose {
     -->
      (118.c:29#2)^guard(! (0-_int32 ==_int32 0));
     -->
      (118.c:29#2)^guard((0-_int32 ==_int32 0));
    }
  }
}


