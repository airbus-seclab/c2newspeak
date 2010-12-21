package t432a is

   subtype T_I is Integer range 1 .. 10;
   subtype T is T_I;

   type TV_I  is array (T_I) of integer;
   type T_V  is array (T) of integer;

   function Mular( LEFT  : in TV_I; RIGHT : Integer) return integer ;
   function Mular( LEFT  : in T_V; RIGHT : Integer ) return integer;
   function "*"  ( LEFT  : in TV_I; RIGHT : Integer) return Integer renames  Mular;


end T432a;
