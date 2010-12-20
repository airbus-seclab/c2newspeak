Warning: 559.c:32#2: dirty cast from integer to pointer accepted
Newspeak output
---------------
void main(void) {
  (559.c:29#7)^ptr ptr;
  (559.c:30#6)^int32 x;
  (559.c:32#2)^ptr =(ptr) (ptr) 4096;
  (559.c:33#2)^x =(int32) [ptr_ptr]32_int32;
}

(TODO!)^4096 : 4 

