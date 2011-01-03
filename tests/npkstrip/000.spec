void (000.c:29#5)^f(void) {
}

void (000.c:32#5)^main(void) {
  (000.c:33#9)^fptr fptr;
  (000.c:34#2)^fptr =(fptr) &_{void -> void}(f);
  (000.c:35#2)^[fptr_fptr]();
}

