void (009.c:1#5)^f(void) {
}

void (009.c:5#5)^g(void) {
  (009.c:7#11)^Ptr (() -> ()) p;
  (009.c:7#11)^p =(fptr) (&_{void -> void}(f) : Ptr (() -> ()));
  (009.c:8#4)^f();
}

