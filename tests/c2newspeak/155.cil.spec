Newspeak Object output
----------------------
155.c
Global used

Global variables

Function definitions
main() {
  int32 x;
  do {
    1- =(int32) 3;
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
155.c
void main(void) {
  (155.c:27#1072)^int32 x;
  (155.c:28#1077)^0- =(int32) 3;
}


