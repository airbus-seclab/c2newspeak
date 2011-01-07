package body T452 is
   procedure Gg is
      X : Integer;
      MSG_ID : T452a.T_D := T452a.T_D'FIRST;
   begin
      case MSG_ID is
         when T452a.C_ACM => X := 1;
         when T452a.M_ACM => null;
      end case;
   end Gg;
end T452;
