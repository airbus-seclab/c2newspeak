typedef struct {
    struct c* cu;
} *p2;


typedef struct {
    p2 sc;
} *p;

typedef struct c {
    int x;
} c;

void f(void){ p cl;
  cl->sc->cu->x;
}

