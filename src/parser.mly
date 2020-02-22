%{
  open Types
%}

%token EOI
%token DEFEQ BAR BRECORD ERECORD LPAREN RPAREN COLON COMMA
%token<Types.variable> VARIABLE
%token<Types.identifier> IDENTIFIER
%token<Types.constructor> CONSTRUCTOR

%start toplevel
%type<Types.parsed_declarations> toplevel
%type<Types.variable list> params
%type<Types.parsed_message list> argssub

%%

toplevel:
| decls=list(msgdecl); EOI { decls }
;
msgdecl:
| name=msgname; params=params; DEFEQ; msg=msg { (name, { pdef_params = params; pdef_main = PGiven(msg); }) }
;
msgname:
| name=IDENTIFIER { name }
;
params:
|                                    { [] }
| LPAREN; x=VARIABLE; tail=paramssub { x :: tail }
;
paramssub:
| RPAREN                            { [] }
| COMMA; RPAREN                     { [] }
| COMMA; x=VARIABLE; tail=paramssub { x :: tail }
;
key:
| key=IDENTIFIER { key }
;
msg:
| x=VARIABLE                      { PVariable(x) }
| name=msgname                    { PName(name, []) }
| name=msgname; LPAREN; msg=msg; tail=argssub { PName(name, msg :: tail) }
| BRECORD; rcd=record; ERECORD    { PRecord(rcd) }
| LPAREN; variant=variant; RPAREN { PVariant(variant) }
;
argssub:
| RPAREN                       { [] }
| COMMA; RPAREN                { [] }
| COMMA; msg=msg; tail=argssub { msg :: tail }
;
record:
|                                             { [] }
| key=key; COLON; msg=msg                     { (key, msg) :: [] }
| key=key; COLON; msg=msg; COMMA; tail=record { (key, msg) :: tail }
;
variant:
| ctor=CONSTRUCTOR; COLON; msg=msg; tail=list(barvariant) { (ctor, Some(msg)) :: tail }
| bv=barvariant; tail=list(barvariant)                    { bv :: tail }
;
barvariant:
| BAR; ctor=CONSTRUCTOR; COLON; msg=msg { (ctor, Some(msg)) }
| BAR; ctor=CONSTRUCTOR;                { (ctor, None) }
;
