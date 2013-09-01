%% @doc
%% Implementation module for the galactic battle simulator.
%% The following example shows the expected behavior of the simulator:
%%
%% Planets=[mercury,uranus,venus, earth]
%% Shields=[mercury,uranus]
%% Alliances=[{mercury, uranus}, {venus, earth}]
%% Actions=[{nuclear,mercury},{laser,venus}, {laser, uranus}]
%%
%% ExpectedSurvivors = [uranus]
%% In order to produce this expected results, the following calls will be tested:
%% * ok = setup_universe(Planets, Shields, Alliances)
%% * [uranus] = simulate_attack(Planets, Actions)
%% * ok = teardown_universe(Planets)
%%
%% All the 3 calls will be tested in order to check they produce the expected
%% side effects (setup_universe/3 creates a process per planet, etc)
%% @end

-module(galaxy_game).

-include_lib("eunit/include/eunit.hrl").

-type planet()::atom().
-type shield()::planet().
-type alliance()::{planet(), planet()}.
-type attack()::{laser | nuclear, planet()}.

-export([setup_universe/3, teardown_universe/1, simulate_attack/2]).

%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->
    io:format("Initial Galaxy: ~p planets~n", [length(Planets)]),
    [spawn_planet(Planet) || Planet <- Planets],
    [setup_shields(Shield) || Shield <- Shields],
    [setup_alliances(Alliance) || Alliance <- Alliances],
    ok.

%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
-spec teardown_universe([planet()]) -> ok.
%% @end
teardown_universe(Planets) ->    
    [teardown_planet(find_planet(Planet)) || Planet <- Planets],
    ok.

%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
    [attack_planet(Attack) || Attack <- Actions],
    timer:sleep(50),
    [Planet || Planet <- Planets, find_planet(Planet) =/= undefined].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Code implemented by challenger.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attack_planet({nuclear, Planet}) ->
    io:format("Enemy Action: The bugs shoot a nuclear cannon at ~p~n", 
              [Planet]),
    exit(find_planet(Planet), kill),
    io:format("Result: ~p is destroyed~n", [Planet]);
attack_planet({laser, Planet}) ->
    io:format("Enemy Action: The bugs shoot a laser cannon at ~p~n", 
              [Planet]),
    exit(find_planet(Planet), laser).

spawn_planet(Planet) ->
    Pid = spawn(fun() -> planet_loop() end),
    erlang:register(Planet, Pid).

setup_shields(Planet) ->
    Pid = find_planet(Planet),
    Pid ! shield_up.

setup_alliances(Alliance) ->
    Planet1 = element(1, Alliance),
    Pid = find_planet(Planet1),
    Pid ! {create_alliance, element(2, Alliance)}.

planet_loop() ->
    receive
        shield_up ->
            process_flag(trap_exit, true),
            planet_loop();
        {'EXIT', _From, Reason} ->
            process_flag(trap_exit, false),
            io:format("Result: Is attacked by ~p, shields down~n", [Reason]),
            planet_loop();
        {create_alliance, Ally} ->
            link(find_planet(Ally)),
            io:format("alliance created~n"),
            planet_loop();
        teardown ->   
            exit(teardown);
        _ -> 
            planet_loop()
    end.

find_planet(PlanetName) ->
    whereis(PlanetName).

teardown_planet(undefined) ->
    ok;
teardown_planet(PlanetId) ->
    PlanetId ! teardown.
