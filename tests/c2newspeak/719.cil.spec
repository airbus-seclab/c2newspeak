Newspeak output
---------------
719.c
int32 f(void) {
  (719.c:27#1064)^!return =(int32) 1;
}

void main(void) {
  int32 tmp;
  (719.c:30#1077)^do {
    (719.c:31#1093)^while (1) {
      (719.c:31#1093)^tmp <- f();
      (719.c:31#1093)^choose {
       -->
        (719.c:31#1093)^guard(! (tmp_int32 ==_int32 0));
       -->
        (719.c:31#1093)^guard((tmp_int32 ==_int32 0));
        (719.c:31#1093)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


