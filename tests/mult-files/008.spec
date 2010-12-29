Warning: toto.h:1#4: multiple definitions of global variable x, in file toto.h variable x should probably be extern accepted
Newspeak output
---------------
void (007-b.c:3#5)^f(void) {
  (007-b.c:4#2)^x =(int32) 1;
}

void (007-a.c:3#5)^main(void) {
  (007-a.c:4#2)^x =(int32) 2;
}

int32 x;

