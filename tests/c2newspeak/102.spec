Warning: 102.c:31#2: dirty cast fptr -> ptr accepted
Newspeak output
---------------
void main(void) {
  (102.c:29#7)^ptr ptr;
  (102.c:31#2)^ptr =(ptr) (ptr <= fptr) &_{(int32) -> void}(f);
}


