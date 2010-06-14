Newspeak output
---------------
745.c
void main(void) {
  (745.c:27#1053)^int32 rnd;
  (745.c:28#1073)^uint32 x;
  (745.c:29#1082)^int32 y;
  (745.c:31#1090)^choose {
   -->
    (745.c:31#1090)^guard(! (rnd_int32 ==_int32 0));
    (745.c:31#1090)^y =(int32) coerce[-2147483648,2147483647] x_uint32;
   -->
    (745.c:31#1090)^guard((rnd_int32 ==_int32 0));
    (745.c:31#1090)^y =(int32) 1024;
  }
}


