--type global/local
package body T052 is

   type T is (A,F,G,H);
   Z :T := A;

   procedure A is
      type T is (A,F,G,H);
      Y : T;
   begin
      Y := A;
   end A;

end T052;
