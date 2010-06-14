Warning: 657.c:29#0: goto statement accepted
Newspeak output
---------------
657.c
void main(void) {
  (657.c:26#6)^uint32 goto.lbl;
  (657.c:26#6)^goto.lbl =(uint32) 0;
  (657.c:26#6)^{
    int32 ij;
    (657.c:27#6)^int32 i;
    (657.c:28#2)^choose {
     -->
      (657.c:28#2)^guard((0 > i_int32));
      (657.c:29#4)^goto.lbl =(uint32) 1;
     -->
      (657.c:28#2)^guard(! (0 > i_int32));
    }
  }
}


