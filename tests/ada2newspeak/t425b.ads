
package T425b is
   subtype R is Integer range 1 .. 3;
   type toto is array (R) of Integer;
   function MUL (  VECTOR_LEFT  : in toto;
                   SCALAR_RIGHT : in Integer) return Toto;

end T425b;
