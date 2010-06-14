Newspeak output
---------------
092.c
void main(void) {
  (092.c:27#6)^int32 j;
  (092.c:28#8)^int16 i;
  (092.c:30#2)^choose {
   -->
    (092.c:30#2)^guard((j_int32 > i_int16));
   -->
    (092.c:30#2)^guard(! (j_int32 > i_int16));
  }
}


