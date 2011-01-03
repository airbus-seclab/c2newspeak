with T434a;
with T434c;
--with T434b;
package body T434 is
Procedure MM is
   Z : T434a.Toto;
   TG : T434c.T_T := T434c.C_C;
begin
   case (TG(1).C_E) is when
     T434a.C_X |  T434a.C_S => null;

   end case;

end MM;
end t434;
