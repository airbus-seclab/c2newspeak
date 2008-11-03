Warning: dirty cast from integer to pointer accepted in 140.c line 32
Newspeak output
---------------
140.c
main() {
  (140.c:30#6)^int32 x;
  (140.c:31#7)^ptr ptr;
  (140.c:32#2)^choose {
    | ((ptr) 1-_int32 ==_ptr 0-_ptr) -->
    | ! ((ptr) 1-_int32 ==_ptr 0-_ptr) -->
  }
}


