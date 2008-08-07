with P1;
package P2 is
--   X : Boolean;

   type Enum is (A, B, C,X);

   function G return P1.Enum;

   function A return Integer;

   procedure Proc(Z : Integer);

end P2;
