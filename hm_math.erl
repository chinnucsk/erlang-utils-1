-module(hm_math).
-export([int32/1,int64/1]).
-export([floor/1,ceil/1]).

int32(Num) -> 
    N1 = Num band 16#FFFFFFFF, 
    case N1 =< 16#7FFFFFFF of
        true ->
			N1; 
        false -> 
			N1 - 16#FFFFFFFF - 1 
    end.

int64(Num) ->
    N1 = Num band 16#FFFFFFFFFFFFFFFF,
    case N1 =< 16#7FFFFFFFFFFFFFFF of
		true ->
			N1;
		false ->
			N1 - 16#FFFFFFFFFFFFFFFF -1
    end.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 ->
			T - 1;
        Pos when Pos > 0 -> 
			T;
        _ -> 
			T
    end.

ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> 
			T;
        Pos when Pos > 0 -> 
			T + 1;
        _ -> 
			T
    end.    
