Newspeak output
---------------
118.c
void main(void) {
  (118.c:27#1072)^int32 x;
  (118.c:28#1081)^int32 y;
  (118.c:29#1086)^choose {
   -->
    (118.c:29#1086)^guard(! (x_int32 ==_int32 0));
   -->
    (118.c:29#1086)^guard((x_int32 ==_int32 0));
    (118.c:29#1086)^choose {
     -->
      (118.c:29#1086)^guard(! (y_int32 ==_int32 0));
     -->
      (118.c:29#1086)^guard((y_int32 ==_int32 0));
    }
  }
}


