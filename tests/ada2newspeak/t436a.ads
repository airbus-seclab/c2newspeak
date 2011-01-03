with T436b;

Package T436a is
   subtype  T_BUS_ID  is t436b.T_BUS_ID;
   function "=" ( LEFT, RIGHT : in T_BUS_ID) return BOOLEAN
     renames t436b."=";
 end T436a;
