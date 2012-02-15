struct s {
    int f;
};

void m (void)
{
    struct s x;
    struct s* p = &x;
    __typeof(*((&p->f))) y;

    y = 0;
}
