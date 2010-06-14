Warning: 559.c:32#1114: dirty cast from integer to pointer accepted
Newspeak output
---------------
559.c
void main(void) {
  (559.c:29#1096)^ptr ptr;
  (559.c:30#1108)^int32 x;
  (559.c:32#1114)^ptr =(ptr) (ptr) 4096;
  (559.c:33#1130)^x =(int32) [ptr_ptr]32_int32;
}


