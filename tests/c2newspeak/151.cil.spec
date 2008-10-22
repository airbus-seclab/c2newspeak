Warning: dirty cast fptr -> ptr accepted in 151.c line 30
Newspeak output
---------------
151.c
main() {
  (151.c:29#1092)^ptr ptr;
  (151.c:30#1100)^0- =(ptr) (ptr <= fptr) &_{int32 -> int32}(f);
}


