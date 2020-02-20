%{
  open Types
%}

%token EOI
%token DEFEQ BAR BRECORD ERECORD LPAREN RPAREN COLON COMMA
%token<Types.identifier> IDENTIFIER
%token<Types.constructor> CONSTRUCTOR

%start toplevel
%type<(Types.identifier * Types.parsed_message) list> toplevel

%%

toplevel:
| decls=list(msgdecl); EOI { decls }
;
msgdecl:
| name=msgname; DEFEQ; msg=msg { (name, msg) }
;
msgname:
| name=IDENTIFIER { name }
;
key:
| key=IDENTIFIER { key }
;
msg:
| name=msgname                    { PName(name) }
| BRECORD; rcd=record; ERECORD    { PRecord(rcd) }
| LPAREN; variant=variant; RPAREN { PVariant(variant) }
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
