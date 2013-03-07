-module(hm_converter).
-export([bianry_to_list/1,list_to_binary/1]).
-export([bool_to_number/1,term_to_iolist/1]).
binary_to_list(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
      Bin -> 
        unicode:characters_to_list(Bin);
      _ ->
        erlang:binary_to_list(Bin)
    end.

list_to_binary(L) when is_binary(L) ->
  L;

list_to_binary(L) when is_list(L) ->
  case unicode:characters_to_binary(L) of
      {error,_,_} -> 
        erlang:list_to_binary(L);
      B ->
         case unicode:characters_to_list(B,utf8) of
            L -> 
              B;
            _ -> 
              erlang:list_to_binary(L)
      end
  end.


bool_to_number(true)->
    1;
bool_to_number(false) ->
    0;
bool_to_number(undefined) ->
    0.

term_to_iolist(Item) when is_binary(Item)->
  Item;
term_to_iolist(Item) when is_atom(Item)->
  erlang:atom_to_binary(Item,utf8);
term_to_iolist(Item) when is_list(Item)->
  hm_misc:list_to_binary(Item);
term_to_iolist(Item) when is_integer(Item)->
  List = erlang:integer_to_list(Item),
  erlang:list_to_binary(List);
term_to_iolist(Item)->
  erlang:term_to_binary(Item).
