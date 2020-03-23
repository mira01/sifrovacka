-type message() :: {text | img, binary()}.
-type messages() :: [message()].

-record(event, {
                 type :: text_message,
                 timestamp :: integer(),
                 sender :: binary(),
                 recipient :: binary(),
                 content :: term()
               }
       ).

