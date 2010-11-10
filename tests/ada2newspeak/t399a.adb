package body t399a is

   procedure A(X : in Integer) is

   begin
      null;
   end A;

   function B(X : in Integer) return Integer is
   begin
      return (2);
   end B;

   procedure A(X : in Float) is
   begin
      null;
   end A;

   function B(X : in Integer) return Float is
   begin
      return (2.0);
   end B;


end t399a ;
