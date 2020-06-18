-module(nmesia_database).

-include("data.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3]).


-record(read, {table :: table(), id :: integer() | nil}).


init(_) ->
    {ok, maps:new()}.

%                  Pid    Map
handle_call(Msg, _From, State) ->
    case Msg of
        #read{table = Table} -> 
            Entry = maps:get(Table, State, []),
            {reply, {ok, Entry}, State};
        _ -> {reply, ok, State}
    end.

   

%%handle_cast(_Msg, State) ->
%%    {noreply, State}