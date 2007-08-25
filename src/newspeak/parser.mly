%{
%}

%token VOID
%token LBRACE RBRACE

%type <unit> program

%start program

%%

program:
                     { () }
;;
