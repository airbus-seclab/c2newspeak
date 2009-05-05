Newspeak output
---------------
681.c
void main(void) {
  (681.c:27#1072)^int32 x;
  (681.c:28#1077)^choose {
   -->
    (681.c:28#1077)^guard(! (0-_int32 ==_int32 0));
    (681.c:28#1090)^0- =(int32) 0;
   -->
    (681.c:28#1077)^guard((0-_int32 ==_int32 0));
  }
}


