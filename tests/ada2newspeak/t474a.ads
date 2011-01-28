package T474a is
   Procedure M ( V : in integer);
   Procedure Q ( V : in Integer; V2 : in integer);
   procedure S( Q1 : in Integer; Q2 : in integer) renames Q;
   procedure S( V : in integer) renames M;
end T474a;
