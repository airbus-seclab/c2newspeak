Warning: 792.c:14#0: goto statement accepted
Newspeak output
---------------
void main(void) {
  (792.c:6#2)^uint32 goto!lbl;
  (792.c:6#2)^goto!lbl =(uint32) 0;
  (792.c:7#10)^{
    ptr x;
    (792.c:11#14)^ptr x.0;
    (792.c:8#4)^[(x_ptr + 48)]8 =(int8) 0;
    (792.c:13#1)^while (1) {
    }
  }
}


