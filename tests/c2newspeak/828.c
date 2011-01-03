struct s {
int n;
int d[];
};

void f(void) {
struct s* ps;
int *pi;
pi = (int *)(ps->d);
}	
