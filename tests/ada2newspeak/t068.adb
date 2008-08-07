procedure T068 is
   type Enum is (A,B,C,D);
   subtype S_Enum is Enum range A..C;
   type Enum2 is new S_Enum;
   X : Enum;
   Y : S_Enum;
   Z : Enum2;
begin
   X := B;
   Y := X;
   Y := A;
   X := Y;
   Z := C;
end T068;
