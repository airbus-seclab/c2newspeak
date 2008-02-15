Warning: Probable invalid cast from pointer to integer in 237.c line 32
Newspeak output
---------------
237.c
main() {
  (237.c:27#1073)^int8;
  (237.c:28#1081)^ptr;
  (237.c:30#1095)^int32;
  (237.c:32#1103)^0- =(int32) (coerce[0,4294967295] 2-_int8 > (uint32) 1-_ptr);
}


