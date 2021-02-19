-module(els_syntax).

-export([convert/1]).

-export([test/1, test2/1]).

convert(ASTList) when is_list(ASTList) ->
  convert_list(ASTList);
convert(empty) ->
  [];
convert(AST) when is_tuple(AST) ->
  Meta = element(2, AST),
  set_meta(convert_1(AST), Meta).

convert_1({record, Meta, Name, Values}) ->
  %% record_expr #name{field = value, ...}
  set_meta(erl_syntax:record_expr(Name, lists:map(fun convert/1, Values)),
           Meta);
convert_1({record, Meta, Expr, Name, Values}) ->
  %% record_expr Expr#name{field = value, ...}
  set_meta(erl_syntax:record_expr(convert(Expr),
                                  Name,
                                  lists:map(fun convert/1, Values)),
           Meta);
convert_1({record_field, Meta, Key, Value}) ->
  %% record_field { field = value }
  set_meta(erl_syntax:record_field(Key, convert(Value)),
           Meta);
convert_1({record_index, Meta, Name, Key}) ->
  %% record_index #name.field
  set_meta(erl_syntax:record_index_expr(Name, Key),
           Meta);
convert_1({record_field, Meta, Name}) ->
  %% record_def_field { field }
  set_meta(erl_syntax:record_field(Name),
           Meta);
convert_1({record_field, Meta, Expr, Name, Key}) ->
  %% record_access Expr#name.field
  set_meta(erl_syntax:record_access(convert(Expr), Name, Key),
           Meta);
convert_1({record_name, Meta, Name}) ->
  %% incomplete #name in macro def body
  set_meta(erl_syntax:tree(record_name, Name),
           Meta);

convert_1({function, _Meta, Clauses}) ->
  {clause, _, {call, _, Name, _}, _, _} = hd(Clauses),
  erl_syntax:function(Name, lists:map(fun convert/1, Clauses));

convert_1({clause, _Meta, Head, GuardOr, Body}) ->
  {call, _, _Name, Patterns} = Head,
  erl_syntax:clause(convert(Patterns), convert(GuardOr), convert_list(Body));
convert_1({guard_or, _Meta, GuardAnds}) ->
  erl_syntax:disjunction(convert_list(GuardAnds));
convert_1({guard_and, _Meta, Guards}) ->
  erl_syntax:conjunction(convert_list(Guards));
convert_1({attribute, _Meta, {atom, _, RawName} = Name, Args}) when RawName =:= type;
                                                                    RawName =:= opaque ->
  %% FIXME what if attribute has different format
  [{op, OpMeta, '::', Type, Definition}] = Args,
  {call, _, TypeName, TypeArgs} = Type,
  erl_syntax:attribute(convert(TypeName),
                       [set_meta(
                          erl_syntax:tuple([convert(TypeName),
                                            convert(Definition),
                                            erl_syntax:list(convert_list(TypeArgs))]),
                          OpMeta)]);
convert_1({attribute, _Meta, {atom, _, RawName} = Name, Args}) when RawName =:= callback;
                                                                    RawName =:= spec ->
  [{spec, _, FName, Clauses}] = Args,
  erl_syntax:attribute(convert(Name), convert_list(Clauses));
convert_1({spec_clause, _, {args, _HeadMeta, Args}, [ReturnType], empty}) ->
  %% FIXME context must be type so that `t()` becames a type not a function application
  erl_syntax:function_type(convert_list(Args), convert(ReturnType));
%%convert_1({spec_clause, _, {args, _HeadMeta, Args}, [Body], Guards}) ->

convert_1({call, _, Name, Args}) ->
  %% context = type
  erl_syntax:type_application(convert(Name), convert_list(Args));
convert_1({remote, _, Mod, Name}) ->
  erl_syntax:module_qualifier(convert(Mod), convert(Name));
convert_1({attribute, _Meta, Name, Args}) ->
  erl_syntax:attribute(convert(Name), Args);
convert_1({macro_call, _Meta, Name, none}) ->
  erl_syntax:macro(convert(Name), none);
convert_1({macro_call, _Meta, Name, Args}) ->
  erl_syntax:macro(convert(Name), convert_list(Args));




convert_1(Tree) ->
  erl_syntax_lib:map_subtrees(fun convert/1, Tree).

convert_list(ASTList) ->
  lists:map(fun convert/1, ASTList).

set_meta(Tree, Meta) ->
  %% FIXME
  erl_syntax:set_pos(Tree, maps:to_list(Meta)).
  %%erl_syntax:set_ann(Tree, [{meta, Meta}]).
  %%erl_syntax:set_ann(Tree, maps:to_list(Meta)).


test(String) ->
  {ok, Forms, _ErrorInfo} = erlfmt:read_nodes_string("nofile", String),
  [erl_syntax_lib:map(fun(T) -> T end, convert(Form))
   || Form <- Forms].

test2(String) ->
  {ok, Forms, _ErrorInfo} = erlfmt:read_nodes_string("nofile", String),
  [els_erlfmt_ast:erlfmt_to_st(Node) || Node <- Forms].
