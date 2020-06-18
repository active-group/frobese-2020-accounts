%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0, put_account/1, put_person/1, unique_account_number/0, unique_person_id/0]).

%% destroy tables in case they already existed
destroy_tables() ->
    mnesia:del_table_copy(person, node()),
    mnesia:del_table_copy(account, node()).

create_tables() ->
    mnesia:create_table(person, [{attributes, record_info(fields, person)}]),
    mnesia:create_table(account, [{attributes, record_info(fields, account)}]).


init_database() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    destroy_tables(),
    create_tables(),
    ok.


write(Obj) ->
    Fun = fun() -> mnesia:write(Obj) end,
    {atomic, ok} = mnesia:transaction(Fun),
    ok.

-spec read_all(mnesia:table()) -> list(tuple()).
read_all(Table) ->
    Fun = fun() -> mnesia:select(Table,[{'_',[],['$_']}]) end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

-spec put_account(#account{}) -> ok.
put_account(Account) ->
    write(Account).

-spec get_all_accounts() -> list(#account{}).
get_all_accounts() ->
    read_all(account).

-spec put_person(#person{}) -> ok.
put_person(Person) ->
    write(Person).

-spec get_all_persons() -> list(#person{}).
get_all_persons() ->
    read_all(person).

-spec unique_person_id() -> unique_id().
unique_person_id() ->
    All = get_all_persons(),
    PersIdsNumbers = lists:map(fun(Pers) -> Pers#person.id end, All),
    next_higher_id(PersIdsNumbers).

-spec unique_account_number() -> account_number().
unique_account_number() ->
    All = get_all_accounts(),
    AccNumbers = lists:map(fun(Acc) -> Acc#account.account_number end, All),
    next_higher_id(AccNumbers).


-spec next_higher_id(list(integer())) -> unique_id().
next_higher_id([]) ->
    0;
next_higher_id(L) ->
    lists:max(L) + 1.
