Newspeak output
---------------
void (198.c:26#5)^main(void) {
  (198.c:27#6)^int32 x;
  (198.c:28#2)^do {
    (198.c:28#2)^while (1) {
      (198.c:28#2)^do {
        (198.c:29#4)^choose {
         -->
          (198.c:29#4)^guard(! (x_int32 ==_int32 0));
          (198.c:30#6)^goto lbl2;
         -->
          (198.c:29#4)^guard((x_int32 ==_int32 0));
        }
        (198.c:28#2)^choose {
         -->
          (198.c:28#2)^guard((10 > x_int32));
         -->
          (198.c:28#2)^guard(! (10 > x_int32));
          (198.c:28#2)^goto lbl1;
        }
      } with lbl2:
    }
  } with lbl1:
}


