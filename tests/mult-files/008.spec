Warning: toto.h:1#4: multiple definitions of global variable x, in file toto.h variable x should probably be extern accepted
Newspeak output
---------------
007-a.c
007-b.c
toto.h
void f(void) {
  (007-b.c:4#2)^x =(int32) 1;
}

void main(void) {
  (007-a.c:4#2)^x =(int32) 2;
}

int32 x;

