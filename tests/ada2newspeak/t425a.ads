with T425b;

package T425a is
   function GET_DATA (X : in Integer ) return T425b.toto ;
   function "*"( VECTOR_LEFT  : in  T425b.toto;
               SCALAR_RIGHT : in  Integer) return T425b.toto renames T425b.MUL;

end T425a;
