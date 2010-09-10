/* Simple ringbuffer-like writes. */

int i;                           // when rb_in -> (rb_data, n),
int rb_data[100];                //         pointer arithmetics ok
int *rb_in;                      // <=>     0 <= n < 100

void main(void)
{
                                 // Loop invariant :
                                 // exists n in [0;99]. rb_in -> (rb_data, n)
  rb_in = rb_data;               // base case : n = 0
  for (i = 0 ; i < 100; i++) {   // induction :
    if (rb_in < rb_data + 97) {  // here (rb_data, n) < (rb_data, 97)
                                 //   hence 0 <= n < 97
      rb_in += 3;                //   (rb_data, n) + 3 = (rb_data, n + 3)
                                 //   with 0 <= n + 3 < 100
    } else {                     // here (rb_data, n) >= rb_data, 97)
                                 //   97 <= n < 100
      rb_in -= 97;               //   (rb_data, n) - 97 = (rb_data, n - 97)
                                 //   with 0 <= n - 97 < 100
    }                            //
  }                              // induction ok
}
