Warning: unknown function execute called: assuming it does not call any function, strip may be incorrect in 007.c line 32
007.c
void f(void) {
}

void main(void) {
  (007.c:32#2)^execute(&_{void -> void}(f));
}

