package body T085 is

   type T is new Integer;

   procedure M is
      type T is new Boolean;
      X : T085.T;

   begin
      X := 3;
   end M;

end T085;
