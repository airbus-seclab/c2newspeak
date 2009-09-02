int x;
void main() {
29:   assert x == 1;
30:   assert x <= 1;
}

Analysis starts
29: x -> [-2147483648, 2147483647] 
29: assertion violation
30: x -> [-2147483648, 2147483647] 
30: assertion violation
Final state: x -> [-2147483648, 2147483647] 
