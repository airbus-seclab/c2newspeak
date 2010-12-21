Warning: 537.c:30#1: dirty cast ptr -> fptr accepted
Newspeak output
---------------
void main(void) {
  (537.c:29#7)^fptr fptr;
  (537.c:30#1)^ptr tmp_cir!0;
  (537.c:30#1)^tmp_cir!0: ptr <- f();
  (537.c:30#1)^fptr =(fptr) (fptr <= ptr) tmp_cir!0_ptr;
}


