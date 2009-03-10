-- Affectation and comparison of the built-in Character type.
--
-- Author : Etienne Millon
-- Date   : Tue Mar 10 2009
--
procedure t232 is
    C : Character;
begin
    -- Character litterals (RM95, A.1.(35) - no Standard.ASCII constants)

    C:=' ';
    C:='!'; 
    C:='"'; 
    C:='#'; 
    C:='$'; 
    C:='%'; 
    C:='&'; 
    C:=''';
    C:='('; 
    C:=')'; 
    C:='*'; 
    C:='+'; 
    C:=','; 
    C:='-'; 
    C:='.'; 
    C:='/';
    C:='0'; 
    C:='1'; 
    C:='2'; 
    C:='3'; 
    C:='4'; 
    C:='5'; 
    C:='6'; 
    C:='7';
    C:='8'; 
    C:='9'; 
    C:=':'; 
    C:=';'; 
    C:='<'; 
    C:='='; 
    C:='>'; 
    C:='?';
    C:='@'; 
    C:='A'; 
    C:='B'; 
    C:='C'; 
    C:='D'; 
    C:='E'; 
    C:='F'; 
    C:='G';
    C:='H'; 
    C:='I'; 
    C:='J'; 
    C:='K'; 
    C:='L'; 
    C:='M'; 
    C:='N'; 
    C:='O';
    C:='P'; 
    C:='Q'; 
    C:='R'; 
    C:='S'; 
    C:='T'; 
    C:='U'; 
    C:='V'; 
    C:='W';
    C:='X'; 
    C:='Y'; 
    C:='Z'; 
    C:='['; 
    C:='\'; 
    C:=']'; 
    C:='^'; 
    C:='_';
    C:='`'; 
    C:='a'; 
    C:='b'; 
    C:='c'; 
    C:='d'; 
    C:='e'; 
    C:='f'; 
    C:='g';
    C:='h'; 
    C:='i'; 
    C:='j'; 
    C:='k'; 
    C:='l'; 
    C:='m'; 
    C:='n'; 
    C:='o';
    C:='p'; 
    C:='q'; 
    C:='r'; 
    C:='s'; 
    C:='t'; 
    C:='u'; 
    C:='v'; 
    C:='w';
    C:='x'; 
    C:='y'; 
    C:='z'; 
    C:='{'; 
    C:='|'; 
    C:='}'; 
    C:='~';

    -- Operators

    -- Two versions are tested : one should be resolved at compile-time
    -- and for the other one, actual code should be generated.

    -- Equality and ordering operators are defined for the
    -- Character type. They should reflect the underlying ASCII values.
    -- See RM95, 4.2.(10)

    -- "="
    if ('a' =  'a') then null; end if;
    if ( C  =  'a') then null; end if;

    -- "/="
    if ('a' /= 'a') then null; end if;
    if ( C  /= 'a') then null; end if;

    -- "<"
    if ('a' <  'a') then null; end if;
    if ( C  <  'a') then null; end if;

    -- "<="
    if ('a' <= 'a') then null; end if;
    if ( C  <= 'a') then null; end if;

    -- ">"
    if ('a' >  'a') then null; end if;
    if ( C  >  'a') then null; end if;

    -- ">="
    if ('a' >= 'a') then null; end if;
    if ( C  >= 'a') then null; end if;

    -- "&" is defined only for non-limited, one-dimensional arrays.

    -- "+" and "-" (in their binary and unary forms), as well as "abs" are
    -- defined only for specific numeric types.

    -- "**" is defined for specific integer types, floating point types and
    -- universal_real only.

    -- "not" is defined for boolean types, modular types, and one-dimensional
    -- array types whose components are of a boolean type only.

    -- "and", "or" and "xor" are only defined for boolean types, modular types,
    -- and one-dimensional array types whose components are of a boolean type.

    -- "*", "/", "mod" and "rem" are only defined for specific integer types.
    
end t232;
