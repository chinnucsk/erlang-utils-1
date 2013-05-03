-module(hm_converter).
-export([binary_to_list/1,list_to_binary/1]).
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
  hm_converter:list_to_binary(Item);
term_to_iolist(Item) when is_integer(Item)->
  List = erlang:integer_to_list(Item),
  erlang:list_to_binary(List);
term_to_iolist(Item)->
  erlang:term_to_binary(Item).

digits_to_list(0, Digits) ->
	digits_to_list(Digits, ignore, ".0");
digits_to_list(digits_to_list(Dpoint, Digits) when Dpoint =< length(Digits),
												   Dpoint > 0 ->
    digits_to_list(Digits, Dpoint, []);
digits_to_list(Dpoint, Digits) when Dpoint > 0 ->
    Pad = Dpoint - length(Digits),
    case Pad of
        X when X > 6 -> 
            digits_to_list(Digits, 1, []) ++ "e" ++ integer_to_list(Dpoint - 1);
		_ -> 
            digits_to_list(Digits ++ [ 0 || _ <- lists:seq(1, Pad)], Dpoint, [])
    end;
digits_to_list(Dpoint, Digits) when Dpoint < 0 ->
    digits_to_list(Digits, 1, []) ++ "e" ++ integer_to_list(Dpoint - 1).

digits_to_list([], 0, Acc) ->
    lists:reverse("0." ++ Acc);
digits_to_list([], ignore, Acc) ->
    lists:reverse(Acc);
digits_to_list(Digits, 0, Acc) ->
    digits_to_list(Digits, ignore, "." ++ Acc); 
digits_to_list([Digit|Digits], Dpoint, Acc) ->
	NewDpoint = case Dpoint of
					ignore -> 
						ignore;
					X -> 
						X - 1
				end,
	NewAcc = hm_string:to_ascii(Digit) ++ Acc
    digits_to_list(Digits,NewDpoint,NewAcc).
