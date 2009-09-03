Warning: unknown function execute called: assuming it does not call any function, strip may be incorrect in 007.c line 32
007.c
void f(void) {
}

void main(void) {
  (007.c:32#2)^fptr execute.arg1;
  (007.c:32#2)^0- =(fptr) &_{void -> void}(f);
  (007.c:32#2)^execute();
}

