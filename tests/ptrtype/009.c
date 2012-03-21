void f(void)
{
}

void g(void)
{
    void (*p)(void) = f;
    (*p)();
}
