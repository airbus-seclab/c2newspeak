-- gnatmake souleve l'erreur
procedure t385 is
    subtype Index is Integer  range 0..9;
    type oo is array (Index) of Integer;
    type CONF  is record
       TITI : oo;
    end record;
    Y : Conf;
begin
   Y:=(TITI=> (others  => 1));

end t385;
