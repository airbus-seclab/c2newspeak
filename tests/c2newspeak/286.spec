Newspeak output
---------------
286.c
void main(void) {
  (286.c:30#7)^i =(int32) 0;
  (286.c:30#14)^j =(int32) 0;
  (286.c:29#5)^do {
    (286.c:30#2)^while (1) {
      (286.c:30#2)^choose {
        | (10 > i_int32) -->
        | ! (10 > i_int32) -->
          (286.c:30#2)^goto lbl0;
      }
      (286.c:30#29)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
      (286.c:30#34)^j =(int32) coerce[-2147483648,2147483647] (j_int32 + 1);
    }
  } with lbl0: {
  }
}

int32 i = 0;
int32 j = 0;

