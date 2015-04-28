-record(timediff, {year=0    :: integer(),
                   month=0   :: integer(),
                   day=0     :: integer(),
                   hour=0    :: integer(),
                   min=0     :: integer(),
                   sec=0     :: integer(),
                   picosec=0 :: integer()}).
-type timediff() :: #timediff{}.
