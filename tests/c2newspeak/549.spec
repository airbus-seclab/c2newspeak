Warning: local composite ceation accepted in 549.c line 32
Warning: missing initializers for structure accepted in 549.c line 31
Newspeak output
---------------
549.c
main() {
  (549.c:28#6)^int32 x;
  (549.c:29#15)^uint32 y;
  (549.c:31#7)^{ uint32 0; int32 0; }32 tmp;
  (549.c:31#7)^0- =(uint32) 1-_uint32;
  (549.c:31#7)^0- =(int32) 0;
  (549.c:31#2)^2- =(int32) 0-_int32;
}


