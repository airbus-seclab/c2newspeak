procedure T489 is
  type Inti is range -2 ** 7  .. 2 ** 7 - 1;
  SH : constant  Inti  := 6;
  type TR is record
     A : Inti;
     B : Inti;
     C : Inti;
     D : Inti;
  end record;

  type T is record
     Y : Integer;
     TRR  :  TR;
  end record;

  A  : T;

begin
   A := (
           Y => 2,
           TRR => (2,5,3,4)
          );

end T489;
