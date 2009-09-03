Warning: unknown function f called: assuming it does not call any function, strip may be incorrect in 001.c line 30
001.c
void main(void) {
  (001.c:29#9)^fptr fptr;
  (001.c:30#2)^0- =(fptr) &_{void -> void}(f);
}

