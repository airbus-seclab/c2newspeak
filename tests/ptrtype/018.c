struct s {
    int a;
    int b;
    float c;
};

void f(void)
{
    struct s x;
    x.b = 0;
    x.b = 1;   // auto-unification
    x.c = 0.0; // heterogenous structure
               // + unification of disjoint fields
}
