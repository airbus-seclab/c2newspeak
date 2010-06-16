Warning: 151.c:30#2: dirty cast fptr -> ptr accepted
Newspeak output
---------------
151.c
void main(void) {
  (151.c:29#7)^ptr ptr;
  (151.c:30#2)^ptr =(ptr) (ptr <= fptr) &_{(int32) -> int32}(f);
}


