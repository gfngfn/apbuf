%{
  open Types
%}

%token EOI
%token<Range.t> DEFEQ BAR BRECORD ERECORD LPAREN RPAREN COLON COMMA
%token<Range.t> META_OUTPUT
%token<Range.t * Types.variable> VARIABLE
%token<Range.t * Types.identifier> IDENTIFIER
%token<Range.t * Types.constructor> CONSTRUCTOR
%token<Range.t * string> STRING

%start toplevel
%type<Types.top_level> toplevel
%type<(Range.t * Types.variable) list> params
%type<Types.parsed_message list> argssub

%%

toplevel:
| meta=meta; decls=list(msgdecl); EOI { (meta, decls) }
;
meta:
| META_OUTPUT; s=STRING; COLON; path=STRING { MetaOutput(s, path) }
;
msgdecl:
| name=msgname; params=params; DEFEQ; msg=msg {
    (name, { pdef_params = params; pdef_main = PGivenNormal(msg); })
  }
| name=msgname; params=params; DEFEQ; variant=variant; {
    (name, { pdef_params = params; pdef_main = PGivenVariant(variant) })
  }
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
