Warning: 786.c:4#0: goto statement accepted
Newspeak output
---------------
786.c
void main(void) {
  (786.c:2#1)^uint32 goto!lbl;
  (786.c:2#1)^goto!lbl =(uint32) 0;
  (786.c:3#6)^{
    int32 q;
    (786.c:2#1)^while (1) {
    }
    (786.c:5#2)^q =(int32) 0;
  }
}


