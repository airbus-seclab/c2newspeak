Newspeak output
---------------
void (286.c:29#5)^main(void) {
  (286.c:30#7)^i =(int32) 0;
  (286.c:30#14)^j =(int32) 0;
  (286.c:30#2)^do {
    (286.c:30#2)^while (1) {
      (286.c:30#2)^choose {
       -->
        (286.c:30#2)^guard((10 > i_int32));
       -->
        (286.c:30#2)^guard(! (10 > i_int32));
        (286.c:30#2)^goto lbl0;
      }
      (286.c:30#29)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
      (286.c:30#34)^j =(int32) coerce[-2147483648,2147483647] (j_int32 + 1);
    }
  } with lbl0:
}

int32 i;
int32 j;

