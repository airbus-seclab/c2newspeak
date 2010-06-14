Newspeak output
---------------
092.c
void main(void) {
  (092.c:27#1072)^int32 j;
  (092.c:28#1083)^int16 i;
  (092.c:30#1089)^choose {
   -->
    (092.c:30#1089)^guard((j_int32 > i_int16));
   -->
    (092.c:30#1089)^guard(! (j_int32 > i_int16));
  }
}


