Newspeak output
---------------
112.c
void main(void) {
  (112.c:27#6)^int32 x;
  (112.c:28#2)^choose {
   -->
    (112.c:28#2)^guard((0-_int32 ==_int32 0));
   -->
    (112.c:28#2)^guard(! (0-_int32 ==_int32 0));
  }
}


