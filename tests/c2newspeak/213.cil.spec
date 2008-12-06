Newspeak output
---------------
213.c
void main(void) {
  int32 tmp;
  (213.c:29#1089)^{
    int32 value_of_f;
    (213.c:29#1089)^f();
    (213.c:29#1089)^1- =(int32) 0-_int32;
  }
  (213.c:28#1073)^do {
    (213.c:29#1089)^choose {
      | ! (0-_int32 ==_int32 0) -->
        (213.c:30#1104)^goto lbl0;
      | (0-_int32 ==_int32 0) -->
    }
  } with lbl0: {
  }
}


