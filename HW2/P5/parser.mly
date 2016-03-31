%token IF ELSE NULL

%start s
%type <int>s

%%

s: IF s t { 0 }
 | NULL { 0 }


t: /* empty */ { 0 }
  | ELSE s { 0 }
