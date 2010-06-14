Newspeak output
---------------
223.c
void main(void) {
  (223.c:27#1072)^int32 x;
  (223.c:28#1077)^choose {
   -->
    (223.c:28#1077)^guard((10 > x_int32));
    (223.c:29#1101)^x =(int32) 1;
   -->
    (223.c:28#1077)^guard(! (10 > x_int32));
  }
}


