void f (void)
{
    int x;
    int *p = &x;
}

void g (void)
{
    int x;
    int *p = &x;
    x = 1;
}

void h (void)
{
    int x;
    int *p = &x;
    *p = 1;
}
