Warning: dirty cast ptr -> fptr accepted in 537.c line 30
Newspeak output
---------------
537.c
main() {
  (537.c:29#7)^fptr fptr;
  (537.c:30#1)^ptr !tmp-1073741822;
  (537.c:30#1)^f();
  (537.c:30#1)^1- =(fptr) (fptr <= ptr) 0-_ptr;
}


