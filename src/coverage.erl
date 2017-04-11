%% Test coverage
-module(coverage).
-export([main/0]).

main() ->
    cover:compile_directory(),
    main:test(),
    cover:analyse_to_file(main,[html]),
    cover:stop().
    

