-module(ct_suite_without_groups_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

%% tests
-compile([export_all]).

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.

init_per_group(_Group, Config) -> Config.
end_per_group(_Group, _Config) -> ok.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config) -> Config.


all() ->
    [
     a1_test,
     a2_test,
     b_test,
     c_test
    ].

a1_test(_Config) -> ok.
a2_test(_Config) -> ok.
b_test(_Config) -> ok.
c_test(_Config) -> ok.
