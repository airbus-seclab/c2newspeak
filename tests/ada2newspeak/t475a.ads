package T475a is
   Procedure M ( I : in integer);
   Procedure Q ( R : Float);
   procedure S( V : in Float) renames Q;
   procedure S( V : in integer) renames M;
end T475a;
