Warning: dirty cast fptr -> ptr accepted in 151.c line 30
Newspeak output
---------------
151.c
main() {
  (151.c:29#7)^ptr ptr;
  (151.c:30#2)^0- =(ptr) (ptr <= fptr) &_{int32 -> int32}(f);
}


