Newspeak output
---------------
115.c
void main(void) {
  (115.c:27#1072)^ptr ptr;
  ptr tmp;
  (115.c:28#1080)^tmp =(ptr) ptr_ptr;
  (115.c:28#1080)^ptr =(ptr) (ptr_ptr + 32);
  (115.c:28#1080)^ptr =(ptr) tmp_ptr;
}


