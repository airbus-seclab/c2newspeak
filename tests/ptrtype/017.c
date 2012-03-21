struct s {
    int a;
    int b;
};

void f(void)
{
    struct s x;
    x.b = 0;
    x.b = 1;   // auto-unification
}
