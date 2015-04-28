-record(message, {fields=[]      :: [{atom(), any()}],
                  body=undefined :: undefined | binary() }).
-type message() :: #message{}.
