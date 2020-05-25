%{
  open Types
%}

%token EOI
%token<Range.t> DEFEQ BAR BRECORD ERECORD LPAREN RPAREN COLON COMMA
%token<Range.t> META_OUTPUT
%token<Range.t * Types.parsed_variable> VARIABLE
%token<Range.t * string> LOWER
%token<Range.t * string> UPPER
%token<Range.t * string> STRING

%start toplevel
%type<((Range.t * Types.parsed_constructor) * Types.parsed_message option) list> variant
%type<Types.top_level> toplevel
%type<(Range.t * Types.parsed_variable) list> params
%type<Types.parsed_message list> argssub

%%

toplevel:
| metas=list(meta); decls=list(msgdecl); EOI { (metas, decls) }
;
meta:
| META_OUTPUT; s=STRING; COLON; path=STRING { MetaOutput(s, path) }
;
msgdecl:
| name=msgname; params=params; DEFEQ; msg=msg {
    (name, { pdef_params = params; pdef_main = PGivenNormal(msg); })
  }
| name=msgname; params=params; DEFEQ; variant=variant {
    (name, { pdef_params = params; pdef_main = PGivenVariant(variant) })
  }
| name=msgname; params=params; DEFEQ; BRECORD; rcd=record; ERECORD {
    (name, { pdef_params = params; pdef_main = PGivenRecord(rcd) })
  }
;
msgname:
| name=LOWER { name }
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
| key=LOWER { key }
;
msg:
| x=VARIABLE                      { PVariable(x) }
| name=msgname                    { PName(name, []) }
| name=msgname; LPAREN; msg=msg; tail=argssub { PName(name, msg :: tail) }
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
| ctor=UPPER; COLON; msg=msg; tail=list(barvariant) { (ctor, Some(msg)) :: tail }
| bv=barvariant; tail=list(barvariant)              { bv :: tail }
;
barvariant:
| BAR; ctor=UPPER; COLON; msg=msg { (ctor, Some(msg)) }
| BAR; ctor=UPPER;                { (ctor, None) }
;
