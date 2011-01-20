with T471a;
package body t471 is
  function "=" ( LEFT: in t471a.R;  RIGHT: in t471a.R ) return BOOLEAN renames t471a."=";
  function "=" ( LEFT: in t471a.S;  RIGHT: in t471a.S ) return BOOLEAN renames t471a."=";
  procedure f is
      C : T := I;
   begin
      if ( C = T'LAST ) then
         null;
      end if;
   end F;
end T471;

