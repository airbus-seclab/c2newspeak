-- sous types vides
procedure t073 is
   subtype St is Integer range 1000..100;
   subtype St2 is st range 495..491;
begin
   null;
end t073;
