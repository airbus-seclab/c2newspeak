struct s {
    int a;
    int x;
    int y;
};

int main(void)
{
    struct s s;
    int *p = &s.x;
}
