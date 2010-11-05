procedure t393 is

   subtype ID is Integer range 1 .. 2;

   type REC is record
        X    : Integer;
        Y    : Integer;
   end record;

   type DATA is array (ID ) of REC;

   C_D_ACM_DATA : constant DATA :=
     (
      1 .. 2 => (X     => 0,
                 Y     => 0 ));

begin
   null;
end t393;
