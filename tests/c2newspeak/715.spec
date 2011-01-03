Newspeak output
---------------
void (715.c:27#5)^main(void) {
  (715.c:28#15)^uint32 y;
  (715.c:30#2)^do {
    (715.c:30#2)^choose {
     -->
      (715.c:30#2)^guard((y_uint32 ==_int32 1));
      (715.c:31#2)^goto lbl1;
     -->
      (715.c:30#2)^guard(! (y_uint32 ==_int32 1));
      (715.c:30#2)^goto lbl1;
    }
  } with lbl1:
}


