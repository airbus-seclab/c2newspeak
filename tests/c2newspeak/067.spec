Newspeak output
---------------
067.c
void f(int32) {
}

void main(void) {
  (067.c:30#6)^int32 a;
  (067.c:31#2)^{
    int32 a;
    (067.c:31#2)^0- =(int32) 1;
    (067.c:31#2)^f();
  }
  (067.c:32#2)^0- =(int32) 3;
}


