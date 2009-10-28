
int x;

void onecpy(char* dst, char* src) {
  *dst = *src;
}

int main() {
  int *p1;
  int *p2;
  
  p1 = &x;
  
  onecpy((char*)&p1, (char*)&p2);
  
  // should be a warning here
  *p2 = 1;
  
  return 0;
}


/* Result of airac:
*************       ./airac5 000.c      ******************


Airac5: public-1.0, analyzing 000.c
= Progress =
Parsing. (0.00s)
Transforming..... (0.00s)
Computing fixpoint with widening. (0.00s)
Computing fixpoint with narrowing (0.00s)
Inspecting result. (0.00s)
Refining alarms (0.00s)

= Input Program Size =
Procedures: 3
Blocks:     5
Commands:   16

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
Total number of alarms: 0
-----------------------------------------------
Airac misses the warning, it is unsound
 */
