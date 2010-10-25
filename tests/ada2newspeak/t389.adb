procedure t389 is

    subtype Index is Integer range 0..2;
    type oo is array (Index, Index) of Integer;
    type CONF is record
    TITI : oo;
    end record;
    Y : Conf;

begin

    Y:=(TITI=> (others =>(others => 1)));

end t389;
