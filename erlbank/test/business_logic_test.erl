-module(business_logic_test).

-include("../src/data.hrl").

-include_lib("eunit/include/eunit.hrl").

setup() ->
    database:init_database(),
    ok.

cleanup(_) -> ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun open_account_test/1]}}.


open_account_test(_) ->
    fun () ->
            ExpectedFirstname = "Test",
            ExpectedLastname = "Ballon",
            Return = business_logic:open_account(ExpectedFirstname,
                                                 ExpectedLastname),
            {ok, Person} = database:get_person(Return#account.person_id),
            ?assertEqual(ExpectedFirstname, (Person#person.firstname)),
            ?assertEqual(ExpectedLastname, (Person#person.surname))
    end.
