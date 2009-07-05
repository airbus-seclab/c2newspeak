Newspeak output
---------------
719.c
int32 f(void) {
  (719.c:27#2)^0- =(int32) 1;
}

void main(void) {
  (719.c:31#2)^int32 !tmp0;
  (719.c:31#2)^do {
    (719.c:31#2)^while (1) {
      (719.c:31#2)^f();
      (719.c:31#2)^choose {
       -->
        (719.c:31#2)^guard(! (0-_int32 ==_int32 0));
       -->
        (719.c:31#2)^guard((0-_int32 ==_int32 0));
        (719.c:31#2)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}


