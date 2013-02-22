% The id server will generate a 64-bit integer base on utc time.
% The id is k-order shortable
% 
%
% Bits                      Description      
% 1      Signeddness flag,always 0.Because thrift only supports signed 64-bit integer And I don't want a negtive integer.
% 41     Unix timestamp,down to the millisecond      
% 10     Top 10 bits of partition number 
% 12     Per-partition static increasing counter
% +------------------+------------------+--------------+
% |0|1     ...     41|42      ...     51|52   ...    63|
% +------------------+------------------+--------------+     
% |0| Unix Timestamp | Partition Number |    Counter   |
%


-module(hm_id_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).
-export ([gen_id/0,gen_id/1]).

-record(state,{partition,
               sequence = 0,
               last_timestamp}).

gen_id(Indentify)->
    gen_server:call(Indentify,next_id).

gen_id()->
    gen_server:call(?MODULE,next_id).

start_link(Args)->
    Name = proplists:get_value(server_name,Args),
    PartitionInteger = proplists:get_value(partition,Args,0),
    Partition = <<PartitionInteger:10>>,
    case  Name of
        pid ->
            gen_server:start_link(?MODULE,Partition,[]);
        Name when erlang:is_tuple(Name) ->
            gen_server:start_link(Name,?MODULE,Partition,[]);
        _->
            gen_server:start_link({local,?MODULE},?MODULE,Partition,[])
    end.

init(Args)->
    TS = erlang:now(),
    {ok,#state{partition = Args,sequence = 0,last_timestamp = TS}}.

handle_call(next_id, From, #state{last_timestamp = TS, sequence = Seq, partition = Partition} = State) ->
    case get_next_seq(TS, Seq) of
        backwards_clock ->
            {reply, {fail, backwards_clock}, State};
        exhausted ->
            %% Retry after a millisecond
            erlang:sleep(1),
            handle_call(next_id, From, State);
        {ok, Time, NewSeq} ->
            {reply, construct_id(Time, Partition, NewSeq), State#state{last_timestamp = Time, sequence = NewSeq}}
    end;

handle_call(_Msg,_From,State)->
    {reply,ok,State}.

handle_cast(_Msg,State)->
    {noreply,State}.

handle_info(_Msg,State)->
    {noreply,State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

get_next_seq({Megas, Secs, Micros} = Time, Seq) ->
    Now = erlang:now(),
    {NowMegas, NowSecs, NowMicros} = Now,
    if
        % Time is essentially equal at the millisecond
        Megas =:= NowMegas,
        Secs =:= NowSecs,
        NowMicros div 1000 =:= Micros div 1000 ->
            case (Seq + 1) rem 4096 of
                0 -> exhausted;
                NewSeq -> {ok, Now, NewSeq}
            end;
        % Woops, clock was moved backwards by NTP
        Now < Time ->
            backwards_clock;
        % New millisecond
        true ->
            {ok, Now, 0}
    end.

construct_id({Megas, Secs, Micros}, Partition, Seq) ->
    Millis = Micros div 1000,
    Combined = (Megas * 1000000 + Secs) * 1000 + Millis,
    <<Integer:64/integer>> = <<0:1, Combined:41/integer-unsigned,
                               Partition:10/bits, Seq:12/integer-unsigned>>,
    Integer.