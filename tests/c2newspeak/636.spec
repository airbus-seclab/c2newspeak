Warning: 636.c:27#1061: goto statement accepted
Newspeak output
---------------
636.c
void main(void) {
  (636.c:26#2)^uint32 goto.lbl;
  (636.c:26#2)^0- =(uint32) 0;
  (636.c:28#4)^0- =(uint32) 1;
  (636.c:28#9)^choose {
   -->
    (636.c:28#9)^guard(0-_uint32);
   -->
    (636.c:28#9)^guard(! 0-_uint32);
  }
}


