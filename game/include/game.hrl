-type message() :: {text | img, binary()}.
-type messages() :: [message()].
-type task_link() :: {puzzle | move, binary()}.
-type task_spec() :: {messages(), Aswer::binary(), NextState::task_link()}.
-type task() :: {binary(), task_spec()}.


-record(task, {name::binary()
              ,assignment::messages()
              ,answer::binary()
              ,next_state::task_link()
              ,hint::messages()
              }).

-record(game, {
          name:: binary(),
          start::task_link(),
          moves::[task()],
          puzzles::[task()]
         }).

-record(event, {
                 type :: text_message,
                 timestamp :: integer(),
                 sender :: binary(),
                 recipient :: binary(),
                 content :: term()
               }
       ).

-type game_spec() :: #game{}.
