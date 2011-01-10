with  T452c;
package T452b is
   subtype T_D is T452c.T_D;
   C_ACM : T_D renames T452c.C_ACM;
   M_ACM : T_D renames T452c.M_ACM;
   function "=" ( LEFT, RIGHT: in T_D ) return BOOLEAN renames T452c."=";
end T452b;
