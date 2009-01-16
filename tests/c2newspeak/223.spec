Newspeak output
---------------
223.c
void main(void) {
  (223.c:27#6)^int32 x;
  (223.c:28#2)^choose {
   -->
    (223.c:28#2)^guard((10 > 0-_int32));
    (223.c:29#4)^0- =(int32) 1;
   -->
    (223.c:28#2)^guard(! (10 > 0-_int32));
  }
}


