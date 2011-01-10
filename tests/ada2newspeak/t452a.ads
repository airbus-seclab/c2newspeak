with  T452b;

package T452a is
    subtype T_D   is T452b.T_D;
    C_ACM   :  T_D renames T452b.C_ACM ;
     M_ACM   :  T_D renames T452b.M_ACM ;
      function "=" ( LEFT, RIGHT: in T_D ) return BOOLEAN renames T452b."=";
end T452a;
