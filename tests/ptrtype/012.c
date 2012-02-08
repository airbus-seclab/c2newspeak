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
