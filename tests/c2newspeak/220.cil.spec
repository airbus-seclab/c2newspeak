Newspeak output
---------------
220.c
void main(void) {
  (220.c:29#1093)^int32 x;
  int32 tmp;
  (220.c:28#1073)^do {
    (220.c:30#1098)^choose {
     -->
      (220.c:30#1098)^guard(! (1-_int32 ==_int32 0));
      (220.c:30#1105)^goto lbl0;
     -->
      (220.c:30#1098)^guard((1-_int32 ==_int32 0));
    }
    (220.c:31#1115)^{
      int32 value_of_f;
      (220.c:31#1115)^f();
      (220.c:31#1115)^1- =(int32) 0-_int32;
    }
    (220.c:31#1115)^choose {
     -->
      (220.c:31#1115)^guard(! (0-_int32 ==_int32 0));
      (220.c:31#1124)^goto lbl0;
     -->
      (220.c:31#1115)^guard((0-_int32 ==_int32 0));
    }
  } with lbl0: {
  }
}


