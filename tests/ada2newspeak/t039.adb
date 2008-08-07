package body T039 is


   procedure Proc_Out(X:out Boolean) is
   begin
      X := True;
   end Proc_Out;


   procedure AppelFonction is
      Y : Boolean;
   begin
      Proc_Out(Y);
   end AppelFonction;

end T039;
