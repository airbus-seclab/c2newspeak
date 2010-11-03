package body T400 is
   function S_MUL (
                   LEFT  : in ARR;
                  RIGHT : in ARR)
                  return  Integer is
   begin
      return( LEFT(1) * RIGHT(3));
   end
     S_MUL;
 end T400;
