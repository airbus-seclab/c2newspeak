Warning: 102.c:31#2: dirty cast fptr -> ptr accepted
Newspeak output
---------------
102.c
void main(void) {
  (102.c:29#7)^ptr ptr;
  (102.c:31#2)^ptr =(ptr) (ptr <= fptr) &_{(int32) -> void}(f);
}


