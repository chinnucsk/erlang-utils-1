-module(hm_string).
-export([to_hex/1,to_ascii/1]).

to_hex([]) ->
    [];
to_hex(Bin) when erlang:is_binary(Bin) -> 
    to_hex(erlang:binary_to_list(Bin));
to_hex(List) when erlang:is_list(List) ->
	to_hex(List,[]).

to_hex([],Acc)->
	lists:reverse(Acc);
to_hex([H|T],Acc)->
	Low = to_ascii(H rem 16),
	High = to_ascii(H div 16),
	Acc1 = [High | Acc],
	Acc2 = [Low | Acc1],
	to_hex(T,Acc2).

to_ascii(N) when N < 10 ->
    $0 + N;
to_ascii(N) ->
    $a + N-10.
