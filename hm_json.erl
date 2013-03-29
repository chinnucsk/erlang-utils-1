%%  jsx example : 
%% 
%%  S = [	
%% 			{<<"name">>, <<"Foo">>}, 
%% 			{<<"activity">>, [
%% 				{<<"name">>, <<"Basketball">>}
%% 				{<<"duration">>, 60},
%% 				{<<"intensity">>, 10}]}]
%% 
%%  get_value(<<"name">>, S)
%%  get_value({<<"activity">>, <<"duration">>}, S)
%%
%%  struct example : 
%% 
%%  S = {struct, [  
%%          {<<"name">>, <<"Foo">>}, 
%%          {<<"activity">>, {struct, [
%%              {<<"name">>, <<"Basketball">>}
%%              {<<"duration">>, 60},
%%              {<<"intensity">>, 10}]}}]}
%% 
%%  get_value(<<"name">>, S)
%%  get_value({<<"activity">>, <<"duration">>}, S)

-module(hm_json).
-export([get_value/2]).

-spec get_value(tuple() | bitstring(), term()) -> term().
get_value(Path, Data) when is_tuple(Path) ->
	L = tuple_to_list(Path),
	get_val(L, Data);
get_value(Key,{struct,List})->
    proplists:get_value(Key, List);
get_value(Key, List) ->
	proplists:get_value(Key, List).

get_val(_, undefined) ->
	undefined;
get_val([Key],Data) ->
	get_value(Key,Data);
get_val([Key|T],Data) ->
	NewData = get_value(Key,Data),
	get_val(T,NewData).

