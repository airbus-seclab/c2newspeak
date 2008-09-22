package body T127 is
   type T is (
     A ,
     B) ;

   for T use (
      A => 0 ,
      B  => 1 ) ;

   procedure P is
   begin
      null;
   end P;

end T127;
