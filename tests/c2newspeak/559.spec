Warning: 559.c:32#2: dirty cast from integer to pointer accepted
Newspeak output
---------------
559.c
void main(void) {
  (559.c:29#7)^ptr ptr;
  (559.c:30#6)^int32 x;
  (559.c:32#2)^1- =(ptr) (ptr) 4096;
  (559.c:33#2)^0- =(int32) [1-_ptr]32_int32;
}

(TODO!)^4096 : 4 

