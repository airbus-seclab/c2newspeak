-- The Add(2)(5) is normally not accepted

package body T204 is
   function Add (X:Integer; Y:Integer) return Integer is
   begin
      return (X+Y);
   end Add;

   procedure OO is
      Xn : Integer;
   begin
      Xn := Add(2)(5);
      --equivalent to Xn := Add(2,5);
   end OO;
end T204;
