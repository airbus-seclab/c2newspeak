void *memcpy (void *dst, void *src, unsigned long n)
{
    char *d = dst;
    char *s = src;
    unsigned long i = 0;
    for (i=0;i<n;i++) {
        d[i] = s[i];
    }
    return dst;
}

void f (void)
{
    int x = 0;
    int y;
    memcpy(&y, &x, sizeof(int));
    int z = y+2;
}
