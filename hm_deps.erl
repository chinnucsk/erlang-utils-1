-module(hm_deps).
-export([get_base_dir/0,get_base_dir/1]).
-export([local_path/1,local_path/2]).
-export([ensure/0,ensure/1]).
-export([deps_on_path/0,new_siblings/1]).
-export([app_name_version/0,app_name_version/1]).

-spec deps_on_path() -> [term()].
deps_on_path() ->
    F = fun (X, Acc) ->
                ProjDir = filename:dirname(X),
                case {filename:basename(X),
                      filename:basename(filename:dirname(ProjDir))} of
                    {"ebin", "deps"} ->                        
                        [filename:basename(ProjDir) | Acc];
                    _ ->
                        Acc
                end
        end,
    ordsets:from_list(lists:foldl(F, [], code:get_path())).
    
-spec new_siblings(module()) -> [term()].
new_siblings(Module) ->
    Existing = deps_on_path(),
    SiblingEbin = filelib:wildcard(local_path(["deps", "*", "ebin"], Module)),
    Siblings = [filename:dirname(X) || X <- SiblingEbin,
                                       ordsets:is_element(
                                         filename:basename(filename:dirname(X)),
                                         Existing) =:= false],
    lists:filter(fun filelib:is_dir/1, 
                 lists:append([[filename:join([X, "ebin"]),
                                filename:join([X, "include"])] ||
                                  X <- Siblings])).
-spec ensure() -> ok.
ensure() ->
    ensure(?MODULE).

-spec ensure(module()) -> ok.                  
ensure(Module) ->
    code:add_paths(new_siblings(Module)),
    code:clash(),
    ok.

-spec get_base_dir() -> string().
get_base_dir() ->
    get_base_dir(?MODULE).

-spec get_base_dir(module()) -> string().
get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).


-spec local_path([string()], module()) -> string().
local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

-spec local_path(term()) -> string().
local_path(Components) ->
    local_path(Components, ?MODULE).

%get appname and version
-spec app_name_version() -> {string(),string()}.
app_name_version()->
	app_name_version(?MODULE).

-spec app_name_version(module()) -> {string(),string()} | error .
app_name_version(Module)->
      BaseDir = get_base_dir(Module),
      BaseName = filename:basename(BaseDir),
      LastMinusPos = string:rchr(BaseName,$-),
      separator(LastMinusPos,BaseName).

-spec separator(integer(),string())-> {string(),string()} | error.
separator(0,_BaseName)->
	error;
separator(Pos,BaseName)->
      AppName = string:substr(BaseName,1,Pos - 1),
      AppVsn = string:substr(BaseName,Pos + 1),
      {AppName,AppVsn}.
