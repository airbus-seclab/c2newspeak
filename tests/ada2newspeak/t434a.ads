with T434b;

Package T434a is
   subtype Toto  is T434b.Toto ;
   C_X  :  Toto renames T434b.C_X;
   C_S  :  Toto renames T434b.C_S;

   function "=" ( LEFT, RIGHT: in  Toto) return BOOLEAN renames t434b."=";
end  T434a;
