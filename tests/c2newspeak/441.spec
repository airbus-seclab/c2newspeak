Warning: 441.c:29: identifier a is defined as a type, avoid using it for another purpose
Warning: 441.c:34: identifier a is defined as a type, avoid using it for another purpose
Newspeak output
---------------
441.c
main() {
  (441.c:33#5)^ptr x;
  (441.c:34#2)^[0-_ptr]32 =(int32) 1;
}


