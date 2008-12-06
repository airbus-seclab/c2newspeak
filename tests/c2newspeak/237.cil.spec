Warning: 237.c:32#1103: dirty cast from pointer to integer accepted
Newspeak output
---------------
237.c
void main(void) {
  (237.c:27#1073)^int8 c;
  (237.c:28#1081)^ptr ptr;
  (237.c:30#1095)^int32 y;
  (237.c:32#1103)^0- =(int32) (coerce[0,4294967295] 2-_int8 > (uint32) 1-_ptr);
}


