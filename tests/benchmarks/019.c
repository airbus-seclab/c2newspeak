void my_memcpy(char *dst, char *src, int n) {
  int i;
  for (i = 0; i < n; i++) {
    dst[i] = src[i];
  }
}

typedef struct {
  int a;
  int *ptr;
} S;

int witness;
int x;
S s1 = {0, &witness};

int main() {
  S s2;
  char t[10];
  witness = 0;
  my_memcpy(&s2, &s1, sizeof(S));
  s2.ptr = &witness;
  *s2.ptr = 10000000;
  // should signal an error since s2.ptr points to witness that is 
  // then set to 1000000
  t[witness] = 0; 
  return 0;
}

/* Airac Results
Airac5: public-1.0, analyzing test.c
= Progress =
Parsing. (0.00s)
Transforming..... (0.00s)
Computing fixpoint with widening. (0.00s)
Computing fixpoint with narrowing (0.00s)
Inspecting result. (0.00s)
Refining alarms (0.00s)

= Input Program Size =
Procedures: 3
Blocks:     17
Commands:   40

= Analysis Parameters =
Analyze every procedure: no
Inlining depth: 1
Unrolling bound: 0
Expected iterations: 300

= Analysis Times =
Total: 0.00s
Parsing: 0.00s
Transforming: 0.00s
Computing fixpoint with widening: 0.00s
Computing fixpoint with narrowing: 0.00s
Inspecting result: 0.00s
Refining alarms: 0.00s

= Alarms =
Total number of alarms: 1

In procedure main():
Number of alarms: 1
test.c:28:17: Offset: [10000000,10000000] Size: [10,10]
 28:     t[witness] = 0;
Callers: main

-----------------
Results ok
*/
