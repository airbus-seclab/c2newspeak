
Cil output for 661.c
--------------------
/* Generated by CIL v. 1.3.5 */
/* print_CIL_Input is false */

#line 26 "661.c"
void main(void) 
{ int x ;

  {
#line 28
  if (x) {

  }
#line 30
  return;
}
}


Newspeak output
---------------
661.c
void main(void) {
  (661.c:27#1053)^int32 x;
  (661.c:28#1058)^choose {
   -->
    (661.c:28#1058)^guard(! (0-_int32 ==_int32 0));
   -->
    (661.c:28#1058)^guard((0-_int32 ==_int32 0));
  }
}


