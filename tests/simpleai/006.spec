int x;
void main() {
29:   assert x == 1;
30:   assert x <= 1;
}

Analysis starts
29: x -> ? 
29: assertion violation
30: x -> ? 
30: assertion violation
Final state: x -> ? 
