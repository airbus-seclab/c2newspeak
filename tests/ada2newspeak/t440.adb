--TODO: le type la variable de boucle
-- FAUX belongs[-2147483648,2147483647]
with T440a;
use T440a;
package body t440 is
   C: constant T_U := 6;
   procedure S is
     begin
        for S in 1 .. C loop
           null;
        end loop;
     end S;
end T440;
