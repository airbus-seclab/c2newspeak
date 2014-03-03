int z;
int y;
int x;
void main() {
29:   z = (x + y);
30:   z = (x - y);
31:   z = (x * y);
32:   z = (x / y);
33:   z = (x % y);
34:   z = (x > y);
35:   z = (x == y);
}

Analysis starts
29: x -> ? y -> ? z -> ? 
29: potential invalid operation: +
30: x -> ? y -> ? z -> ? 
30: potential invalid operation: -
31: x -> ? y -> ? z -> ? 
31: potential invalid operation: *
32: x -> ? y -> ? z -> ? 
32: potential invalid operation: /
33: x -> ? y -> ? z -> ? 
33: potential invalid operation: %
34: x -> ? y -> ? z -> ? 
35: x -> ? y -> ? z -> ? 
Final state: x -> ? y -> ? z -> ? 
