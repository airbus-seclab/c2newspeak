Warning: 617.c:30#1106: goto statement accepted
Newspeak output
---------------
617.c
void main(void) {
  (617.c:27#4)^uint32 continue.617.c:29#13.0;
  (617.c:27#4)^0- =(uint32) 0;
  (617.c:27#4)^{
    uint32 goto.lbl;
    (617.c:27#4)^0- =(uint32) 0;
    (617.c:27#4)^while (1) {
      (617.c:27#13)^while (1) {
        (617.c:29#13)^1- =(uint32) 1;
      }
      (617.c:27#4)^do {
        (617.c:27#13)^choose {
         -->
          (617.c:27#13)^guard(1-_int32);
          (617.c:27#13)^1- =(uint32) 0;
          (617.c:27#13)^goto lbl3;
         -->
          (617.c:27#13)^guard(! 1-_int32);
        }
      } with lbl3: {
      }
    }
  }
}


