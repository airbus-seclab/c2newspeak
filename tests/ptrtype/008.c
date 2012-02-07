// like 007 but changing names so that type inference is made in opposite order

int f (void)
{
    return 1;
}

void g(void)
{
    int x = f();
}
