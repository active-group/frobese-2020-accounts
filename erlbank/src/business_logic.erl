%% This module represents the business logic layer

-module(business_logic).

-include("data.hrl").

-export([convert/1, get_all_events/0, open_account/2]).

%% Opens an account, that is creates a new account containing a new person
%% Writes them into database.

-spec open_account(binary(), binary()) -> #account{}.

open_account(Firstname, Surname) ->
    make_account(make_person(Firstname, Surname)).

-spec make_person(binary(), binary()) -> #person{}.

make_person(Firstname, Surname) ->
    PersId = database:unique_person_id(),
    Pers = #person{id = PersId, firstname = Firstname,
                   surname = Surname},
    database:put_person(Pers),
    Pers.

-spec make_account(#person{}) -> #account{}.

make_account(Person) ->
    AccNr = database:unique_account_number(),
    Acc = #account{account_number = AccNr,
                   person_id = Person#person.id, amount = 1000},
    database:put_account(Acc),
    accounts_server:send_event(Acc),
    Acc.

% kloppt den Account mit Person in eine Map
-spec convert(#account{}) -> map().

convert(Account = #account{}) ->
    case database:get_person(Account#account.person_id) of
        {ok, Person} ->
            {ok,
             #{account_number => Account#account.account_number,
               amount => Account#account.amount,
               firstname => Person#person.firstname,
               surname => Person#person.surname}};
        _ -> error
    end.

-spec get_all_events() -> [map()].

get_all_events() ->
    Accounts = database:get_all_accounts(),
    lists:map(fun (Account) ->
                      {ok, Event} = convert(Account),
                      Event
              end,
              Accounts).
