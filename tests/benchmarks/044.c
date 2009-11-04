struct s{
  char a,b,c,d;
};

void scopy (struct s*dst, struct s*src) {
  char*p;
#if 1
  p = &src->a;
#else
  p = src;
#endif
  for (i=0 ; i<4 ; i++) {
    dst[i] = *p;
    p++;
  }
}

