-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().
-type table() :: person | account | transaction.

-record(person, 
    {id :: unique_id(), 
     firstname :: binary(),
     surname :: binary()}).
-record(account,
    {account_number :: account_number(),
     person_id :: unique_id(),
     amount :: money()}).
-record(transaction, 
    {id :: unique_id(), 
     timestamp, 
     from_acc_nr :: account_number(), 
     to_acc_nr :: account_number(), 
     amount :: money()}).

