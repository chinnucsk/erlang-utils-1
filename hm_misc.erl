%%-*- coding:utf-8 -*-
-module(hm_misc).

%%-compile({no_auto_import,[binary_to_list/1,list_to_binary/1]}).

-export([monitor/2,demonitor/2]).
-export([to_tuplelist/2,to_record/3,to_match_record/3]).
-export([ensure_started/1,get_env/2,get_env/3,set_env/3]).
-export([first_in_list/1,nth_in_list/2]).

%% Tow functions to deal with pid monitor and demonitor in ets table
monitor(Pid,PidGroup) ->
    case ets:match_object(PidGroup, {Pid,'_'}) of
        [] ->
            M = erlang:monitor(process, Pid),
            ets:insert(PidGroup, {Pid, M});
        _ ->
            %% Already monitoring peer
            ok
    end.

demonitor(Pid,PidGroup) ->
    case ets:match_object(PidGroup,{Pid,'_'}) of
    	 [{Pid,Ref}] ->
	    erlang:demonitor(Ref),
	    ok;
         [] ->
	    ok
   end.

%% Some functions deal with record 
to_tuplelist(Fields,Record)->
    Values = tl(tuple_to_list(Record)),
    lists:zip(Fields,Values).

to_record(Name,Fields,TupleList)->
    Values = lists:map(
	       fun(F)->
		       case lists:keyfind(F,1,TupleList) of
			   false ->
			       undefined;
			   {_,V} ->
			       V
			end
	       end,Fields),
    Record = [Name] ++ Values ,
    erlang:list_to_tuple(Record).

to_match_record(Name,Fields,TupleList)->
    Values = lists:map(
	       fun(F)->
		       case lists:keyfind(F,1,TupleList) of
			   false ->
			       '_';
			   {_,V} ->
			       V
			end
	       end,Fields),
    Record = [Name] ++ Values ,
    erlang:list_to_tuple(Record).

%% Application ensure start
ensure_started(App) ->
        case application:start(App) of
            ok ->
                ok;
            {error, {already_started, App}} ->
                ok
        end.

get_env(AppName,Par) ->
    case application:get_env(AppName,Par) of
        undefined ->
            undefined;
        {ok,Val} ->
            Val
    end.

get_env(AppName,Par,DefVal) ->
    case get_env(AppName,Par) of
        undefined ->
            DefVal;
        Val ->
            Val
    end.

set_env(AppName,Par,Val)->
    application:set_env(AppName,Par,Val).

%% String tools

%%judge tools

first_in_list(List)->
    nth_in_list(1,List).
nth_in_list(_N,[])->
    undefined;
nth_in_list(1,[H|_])->
    H;
nth_in_list(N,[_|T])->
    nth_in_list(N-1,T).
