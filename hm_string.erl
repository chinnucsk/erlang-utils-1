-module(hm_string).
-export([digit_to_ascii/1]).
-export([integers_to_hex/1,digits_to_float/2]).

integers_to_hex([]) ->
    [];
integers_to_hex(Bin) when erlang:is_binary(Bin) -> 
    integers_to_hex(erlang:binary_to_list(Bin));
integers_to_hex(List) when erlang:is_list(List) ->
	integers_to_hex(List,[]).

integers_to_hex([],Acc)->
	lists:reverse(Acc);
integers_to_hex([H|T],Acc)->
	Low = digit_to_ascii(H rem 16),
	High = digit_to_ascii(H div 16),
	Acc1 = [High | Acc],
	Acc2 = [Low | Acc1],
	integers_to_hex(T,Acc2).

digit_to_ascii(N) when N < 10 ->
    $0 + N;
digit_to_ascii(N) ->
    $a + N-10.

digits_to_float(0, Digits) ->
	digits_to_float(Digits, ignore, ".0");
digits_to_float(Dpoint, Digits) when Dpoint =< length(Digits),
									  Dpoint > 0 ->
    digits_to_float(Digits, Dpoint, []);
digits_to_float(Dpoint, Digits) when Dpoint > 0 ->
    Pad = Dpoint - length(Digits),
    case Pad of
        X when X > 6 -> 
            digits_to_float(Digits, 1, []) ++ "e" ++ erlang:integer_to_list(Dpoint - 1);
		_ -> 
            digits_to_float(Digits ++ [ 0 || _ <- lists:seq(1, Pad)], Dpoint, [])
    end;
digits_to_float(Dpoint, Digits) when Dpoint < 0 ->
    digits_to_float(Digits, 1, []) ++ "e" ++ erlang:integer_to_list(Dpoint - 1).

digits_to_float([], 0, Acc) ->
    lists:reverse("0." ++ Acc);
digits_to_float([], ignore, Acc) ->
    lists:reverse(Acc);
digits_to_float(Digits, 0, Acc) ->
    digits_to_float(Digits, ignore, "." ++ Acc); 
digits_to_float([Digit|Digits], Dpoint, Acc) ->
	NewDpoint = case Dpoint of
					ignore -> 
						ignore;
					X -> 
						X - 1
				end,
	NewAcc = [digit_to_ascii(Digit)] ++ Acc,
    digits_to_float(Digits,NewDpoint,NewAcc).
