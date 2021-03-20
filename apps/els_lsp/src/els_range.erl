-module(els_range).

-include("els_lsp.hrl").

-export([ compare/2
        , in/2
        , range/4
        ]).

-spec compare(poi_range(), poi_range()) -> boolean().
compare( #{from := FromA, to := ToA}
       , #{from := FromB, to := ToB}
       ) when FromB =< FromA, ToA =< ToB; %% Nested
              ToA =< FromB;               %% Sequential
              FromA =< FromB, ToA =< ToB  %% Sequential & Overlapped
              ->
  true;
compare(_, _) ->
  false.

-spec in(poi_range(), poi_range()) -> boolean().
in(#{from := FromA, to := ToA}, #{from := FromB, to := ToB}) ->
  FromA >= FromB andalso ToA =< ToB.

-spec range(pos() | {pos(), pos()}, poi_kind(), any(), any()) -> poi_range().
range(Anno, _Type, _Id, _Data) when is_map(Anno) ->
  %% Recommenting can modify the start and end locations of certain trees
  %% see erlfmt_recomment:put_(pre|post)_comments/1
  From =
    case maps:is_key(pre_comments, Anno) of
      true ->
        maps:get(inner_location, Anno);
      false ->
        maps:get(location, Anno)
    end,
  To =
    case maps:is_key(post_comments, Anno) of
      true ->
        maps:get(inner_end_location, Anno);
      false ->
        maps:get(end_location, Anno)
    end,
  #{ from => From, to => To };
range({{_Line, _Column} = From, {_ToLine, _ToColumn} = To}, Name, _, _Data)
  when Name =:= export;
       Name =:= export_type;
       Name =:= spec ->
  %% range from unparsable tokens
  #{ from => From, to => To };
range({Line, Column}, function_clause, {F, _A, _Index}, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(F)),
  #{ from => From, to => To }.

-spec plus(pos(), string()) -> pos().
plus({Line, Column}, String) ->
  {Line, Column + string:length(String)}.

-spec atom_to_string(atom()) -> string().
atom_to_string(Atom) ->
  io_lib:write(Atom).
