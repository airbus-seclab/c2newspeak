Newspeak output
---------------
420.c
void main(void) {
  (420.c:30#1083)^i =(int32) 0;
  (420.c:30#1083)^j =(int32) 0;
  (420.c:29#1067)^do {
    (420.c:30#1083)^while (1) {
      (420.c:30#1083)^choose {
        | (10 > i_int32) -->
        | ! (10 > i_int32) -->
          (420.c:30#1083)^goto lbl0;
      }
      (420.c:30#1083)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
    }
  } with lbl0: {
  }
}

int32 i = 0;
int32 j = 0;

