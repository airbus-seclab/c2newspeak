struct si {
    int i;
};

union u {
    struct si si;
};

void f (void)
{
    union u x = { .si = { .i = 0 } };
}
