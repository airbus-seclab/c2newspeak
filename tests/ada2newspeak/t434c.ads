with T434a;

package T434c is
    type T_C is
      record
         C_E         : T434a.Toto;
         A         : Boolean;
      end record;

   subtype idx is Integer range 1..3;
   type T_T is array (idx) of T_C;

   C_C : constant  T_T := ( ( T434a.C_S, true),( T434a.C_S, true),( T434a.C_S, true) );
end T434c;
