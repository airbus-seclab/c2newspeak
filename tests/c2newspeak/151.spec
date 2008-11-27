Warning: 151.c:30: dirty cast fptr -> ptr accepted
Newspeak output
---------------
151.c
main() {
  (151.c:29#7)^ptr ptr;
  (151.c:30#2)^0- =(ptr) (ptr <= fptr) &_{int32 -> int32}(f);
}


