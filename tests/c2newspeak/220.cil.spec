Newspeak output
---------------
220.c
void main(void) {
  (220.c:29#1093)^int32 x;
  int32 tmp;
  (220.c:28#1073)^do {
    (220.c:30#1098)^choose {
     -->
      (220.c:30#1098)^guard(! (x_int32 ==_int32 0));
      (220.c:30#1105)^goto lbl0;
     -->
      (220.c:30#1098)^guard((x_int32 ==_int32 0));
    }
    (220.c:31#1115)^tmp <- f();
    (220.c:31#1115)^choose {
     -->
      (220.c:31#1115)^guard(! (tmp_int32 ==_int32 0));
      (220.c:31#1124)^goto lbl0;
     -->
      (220.c:31#1115)^guard((tmp_int32 ==_int32 0));
    }
  } with lbl0: {
  }
}


