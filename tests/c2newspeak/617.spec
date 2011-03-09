Warning: 617.c:30#6: goto statement accepted
Newspeak output
---------------
void (617.c:26#5)^main(void) {
  (617.c:27#4)^uint32 continue.617.c:29#13.0;
  (617.c:27#4)^continue.617.c:29#13.0 =(uint32) 0;
  (617.c:27#4)^{
    uint32 goto!lbl;
    (617.c:27#4)^goto!lbl =(uint32) 0;
    (617.c:27#4)^while (1) {
      (617.c:29#13)^continue.617.c:29#13.0 =(uint32) 1;
      (617.c:27#4)^do {
        (617.c:27#13)^choose {
         -->
          (617.c:27#13)^guard(continue.617.c:29#13.0_int32);
          (617.c:27#13)^continue.617.c:29#13.0 =(uint32) 0;
          (617.c:27#13)^goto lbl3;
         -->
          (617.c:27#13)^guard(! continue.617.c:29#13.0_int32);
        }
      } with lbl3:
    }
  }
}


