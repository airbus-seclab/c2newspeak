-- Case .. when statement : alternation in a single discrete_choice_list
--
-- Author : Etienne Millon
-- Date   : Mon Mar  9 2009
--
procedure t230 is
    X : Integer range 0..2 := 0;
    Y : Integer;
begin
    case X is
        when 0|1 => Y := 5;
        when 2   => Y := 3;
    end case;
end t230;
