Newspeak output
---------------
void main(void) {
  (118.c:27#6)^int32 x;
  (118.c:28#6)^int32 y;
  (118.c:29#2)^choose {
   -->
    (118.c:29#2)^choose {
     -->
      (118.c:29#2)^guard(! (x_int32 ==_int32 0));
     -->
      (118.c:29#2)^guard((x_int32 ==_int32 0));
      (118.c:29#2)^guard(! (y_int32 ==_int32 0));
    }
   -->
    (118.c:29#2)^guard((x_int32 ==_int32 0));
    (118.c:29#2)^guard((y_int32 ==_int32 0));
  }
}


