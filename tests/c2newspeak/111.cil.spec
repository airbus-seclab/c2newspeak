Newspeak output
---------------
111.c
void main(void) {
  (111.c:27#1072)^int32 x;
  (111.c:27#1075)^int32 y;
  (111.c:28#1080)^choose {
   -->
    (111.c:28#1080)^guard(! (x_int32 ==_int32 0));
    (111.c:28#1080)^choose {
     -->
      (111.c:28#1080)^guard(! (y_int32 ==_int32 0));
     -->
      (111.c:28#1080)^guard((y_int32 ==_int32 0));
    }
   -->
    (111.c:28#1080)^guard((x_int32 ==_int32 0));
  }
}


