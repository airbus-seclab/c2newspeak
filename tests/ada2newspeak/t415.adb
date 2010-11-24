with T415a;
Use T415a;

package body T415  is
   procedure B (A : in T415a.T) is begin
      if (A = F) then
         null;
      end if;
   end;

end t415;
