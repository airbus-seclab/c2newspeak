package body T444 is
   A : UINT16;
   B : AA;
   procedure donull is
   begin
      A := B'Length;
      --   A := INTEGER ( AA'Length ) ;
   end donull;
end T444;
