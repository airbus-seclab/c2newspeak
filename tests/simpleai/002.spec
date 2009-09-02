int x;
int y;
int z;
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
29: ?
29: potential invalid operation: +
30: ?
30: potential invalid operation: -
31: ?
31: potential invalid operation: *
32: ?
32: potential invalid operation: /
33: ?
33: potential invalid operation: %
34: ?
34: potential invalid operation: >
35: ?
35: potential invalid operation: ==
Final state: ?
