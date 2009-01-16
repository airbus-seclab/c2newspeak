Newspeak output
---------------
198.c
void main(void) {
  (198.c:27#6)^int32 x;
  (198.c:28#2)^do {
    (198.c:29#4)^choose {
     -->
      (198.c:29#4)^guard(! (0-_int32 ==_int32 0));
      (198.c:30#6)^goto lbl2;
     -->
      (198.c:29#4)^guard((0-_int32 ==_int32 0));
    }
  } with lbl2: {
  }
  (198.c:28#2)^do {
    (198.c:28#2)^while (1) {
      (198.c:28#2)^choose {
       -->
        (198.c:28#2)^guard((10 > 0-_int32));
       -->
        (198.c:28#2)^guard(! (10 > 0-_int32));
        (198.c:28#2)^goto lbl1;
      }
      (198.c:28#2)^do {
        (198.c:29#4)^choose {
         -->
          (198.c:29#4)^guard(! (0-_int32 ==_int32 0));
          (198.c:30#6)^goto lbl3;
         -->
          (198.c:29#4)^guard((0-_int32 ==_int32 0));
        }
      } with lbl3: {
      }
    }
  } with lbl1: {
  }
}


