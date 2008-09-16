--type global/local
package body T116 is

   type T is (A,F,G,H);
   Z : T := A;

   procedure A is
      type T is (A,F,G,H);
      Y : T;
   begin
      Y := T116.Z;
   end A;

end T116;
