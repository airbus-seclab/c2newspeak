Warning: 537.c:30#1: dirty cast ptr -> fptr accepted
Newspeak output
---------------
537.c
void main(void) {
  (537.c:29#7)^fptr fptr;
  (537.c:30#1)^ptr !tmp0;
  (537.c:30#1)^f();
  (537.c:30#1)^1- =(fptr) (fptr <= ptr) 0-_ptr;
}


