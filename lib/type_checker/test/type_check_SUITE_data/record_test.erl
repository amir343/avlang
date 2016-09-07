-module(record_test).

-record(my_record, { id
                   , name
                   , tel}).

-record(telephone, { area_code
                   , number }).

type #my_record :: {Integer, String, #telephone{}}.
type #telephone :: {Integer, Integer}.


record(#my_record{id = Id, tel = Tel}) ->
  TT = Id,
  #my_record{id = TT, name = my_last_name(), tel = #telephone{area_code = 98
                                                             , number = 92323}}.

my_last_name() ->
  "Amir Moulavi".

test_record() ->
  record(#my_record{id = 13, name = "amir", tel = #telephone{area_code = 97
                                                            , number = 254245}}).
