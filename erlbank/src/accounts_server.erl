-module(accounts_server).

-behavior(gen_server).

-export([init/1, handle_cast/2, handle_call/3, start/0, send_event/1, start_link/0]).


init(InitState) ->
    {ok, InitState}. 

start() ->
    gen_server:start({global, accounts}, ?MODULE, [], []).
                                                % ^ wird an init übergeben

handle_cast(Request, State) ->
    case Request of
        {register, Sender} -> {noreply,[Sender | State]};
        {replay, Sender} -> 
            Sender ! {replay, business_logic:get_all_events()},
            {noreply, State};
        {publish, Account} ->
            {ok, Event} = business_logic:convert(Account), 
            lists:foreach(fun(Rec) -> Rec ! {new, Event} end, State),
            {noreply, State}
        end.

handle_call(_Request, _From, State) ->
    {reply, {}, State}.

send_event(Account) -> 
    gen_server:cast({global, accounts}, {publish, Account}).

start_link() ->
    gen_server:start_link({global, accounts}, ?MODULE, [], []).