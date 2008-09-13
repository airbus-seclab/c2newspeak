Warning: identifier a is defined as a type, avoid using it for another purpose in 441.c line 29
Warning: identifier a is defined as a type, avoid using it for another purpose in 441.c line 34
Newspeak output
---------------
441.c
main() {
  (441.c:33#1119)^ptr x;
  (441.c:34#1124)^[0-_ptr]32 =(int32) 1;
}


