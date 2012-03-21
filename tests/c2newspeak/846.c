struct s {
    int f;
};

void m (void)
{
    struct s x;
    struct s* p = &x;
    __typeof__(*(((&p->f)))) y;
    y = 0;
}
