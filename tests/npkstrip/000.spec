000.c
f() {
}

main() {
  (000.c:33#9)^fptr fptr;
  (000.c:34#2)^0- =(fptr) &_{void -> void}(f);
  (000.c:35#2)^[0-_fptr]();
}

