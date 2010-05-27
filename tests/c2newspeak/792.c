struct s {
  int n;
};

void main(void) {
  {
    char *x;
    x[6] = 0;
  }
  {
    struct s* x;
  }  
 lbl:;
  goto lbl;
 
}
