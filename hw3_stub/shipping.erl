% Pranav Yalamala and Yashvardhan Sharma
% I pledge my honor that I have abided by the Stevens Honor System.

-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
    Ship = lists:keyfind(Ship_ID, 2, Shipping_State#shipping_state.ships),
    case Ship of
        false -> error;
        _ -> Ship
    end.

get_container(Shipping_State, Container_ID) ->
    Container = lists:keyfind(Container_ID, 2, Shipping_State#shipping_state.containers),
    case Container of
        false -> error;
        _ -> Container
    end.

get_port(Shipping_State, Port_ID) ->
    Port = lists:keyfind(Port_ID, 2, Shipping_State#shipping_state.ports),
    case Port of
        false -> error;
        _ -> Port
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    lists:filtermap(fun(Shipping_location) ->
                            case Shipping_location of
                                {Port_ID, Dock, _Ship_ID} -> {true, Dock};
                                _ -> false
                            end
                    end, Shipping_State#shipping_state.ship_locations).

get_ship_location(Shipping_State, Ship_ID) ->
    Ship_locations = lists:keyfind(Ship_ID, 3, Shipping_State#shipping_state.ship_locations),
    case Ship_locations of
        {Port_ID, Dock, Ship_ID} -> {Port_ID, Dock};
        _ -> error
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    Container_Weights = lists:map(fun(Container_ID) ->
                                      Container = get_container(Shipping_State, Container_ID),
                                      case Container of
                                          error -> error;
                                          #container{id = Container_ID, weight = Weight} -> Weight
                                      end
                                  end, Container_IDs),
    case lists:member(error, Container_Weights) of
        true -> error;
        false -> lists:sum(Container_Weights)
    end.

get_ship_weight(Shipping_State, Ship_ID) ->
    Ship  = get_ship(Shipping_State, Ship_ID),
    case Ship of
         error -> error;
         _ -> 
            Shipping_inventory = Shipping_State#shipping_state.ship_inventory,
            case maps:find(Ship_ID, Shipping_inventory) of
                error -> error;
                {ok, Container_IDs} -> get_container_weight(Shipping_State, Container_IDs)
            end
    end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship_Port = get_ship_location(Shipping_State, Ship_ID),
    case Ship_Port of
        error -> error;
        {Port_ID, _Dock} ->
            Port_inventory = Shipping_State#shipping_state.port_inventory,
            case maps:find(Port_ID, Port_inventory) of
                error -> error;
                {ok, Port_Containers} ->
                    case is_sublist(Port_Containers, Container_IDs) of
                        true -> 
                            Port_Containers_update = lists:subtract(Port_Containers, Container_IDs),
                            New_Port_Inventory = maps:update(Port_ID, Port_Containers_update, Port_inventory),
                            Ship_Containers = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
                            Ship_Containers_update = Ship_Containers ++ Container_IDs,
                            New_Ship_Inventory = maps:update(Ship_ID, Ship_Containers_update, Shipping_State#shipping_state.ship_inventory),
                            New_Shipping_State = Shipping_State#shipping_state{port_inventory = New_Port_Inventory, ship_inventory = New_Ship_Inventory},
                            {ok, New_Shipping_State};
                        false -> error
                    end
            end
    end.

unload_ship_all(Shipping_State, Ship_ID) ->
    Ship_Port = get_ship_location(Shipping_State, Ship_ID),
    case Ship_Port of
        error -> error;
        {Port_ID, _Dock} ->
            Port_inventory = Shipping_State#shipping_state.port_inventory,
            case maps:find(Port_ID, Port_inventory) of
                error -> error;
                {ok, Port_Containers} ->
                    Ship_Containers = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
                    Port_Containers_update = Port_Containers ++ Ship_Containers,
                    New_Port_Inventory = maps:update(Port_ID, Port_Containers_update, Port_inventory),
                    New_Ship_Inventory = maps:update(Ship_ID, [], Shipping_State#shipping_state.ship_inventory),
                    New_Shipping_State = Shipping_State#shipping_state{port_inventory = New_Port_Inventory, ship_inventory = New_Ship_Inventory},
                    {ok, New_Shipping_State}
            end
        end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Ship_Port = get_ship_location(Shipping_State, Ship_ID),
    case Ship_Port of
        error -> error;
        {Port_ID, _Dock} ->
             Port_inventory = Shipping_State#shipping_state.port_inventory,
             case maps:find(Port_ID, Port_inventory) of
                    error -> error;
                    {ok, Port_Containers} ->
                        Ship_Containers = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),
                        case is_sublist(Ship_Containers, Container_IDs) of
                            true ->
                                Port_Containers_update = Port_Containers ++ Container_IDs,
                                New_Port_Inventory = maps:update(Port_ID, Port_Containers_update, Port_inventory),
                                Ship_Containers_update = lists:subtract(Ship_Containers, Container_IDs),
                                New_Ship_Inventory = maps:update(Ship_ID, Ship_Containers_update, Shipping_State#shipping_state.ship_inventory),
                                New_Shipping_State = Shipping_State#shipping_state{port_inventory = New_Port_Inventory, ship_inventory = New_Ship_Inventory},
                                {ok, New_Shipping_State};
                            false -> error
                        end
                end
        end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    Ship_port = get_ship_location(Shipping_State, Ship_ID),
    case Ship_port of
        error -> error;
        {Old_Port_ID, Old_Dock} ->
            case {Old_Port_ID, Old_Dock} == {Port_ID, Dock} of
                true -> error;
                false ->
                    Ship_locations = Shipping_State#shipping_state.ship_locations,
                    All_ports = lists:filtermap(fun(Shipping_location) ->
                                                    case Shipping_location of
                                                        {Port_ID, Dock, _S} -> true;
                                                        _ -> false
                                                    end
                                                end, Ship_locations),
                    case All_ports of
                        [] -> 
                            Ship_Locations_update = lists:map(fun({P_ID, D, S_ID}) ->
                                                                       case S_ID == Ship_ID of
                                                                           true -> {Port_ID, Dock, Ship_ID};
                                                                           false -> {P_ID, D, S_ID}
                                                                       end
                                                                   end, Ship_locations),
                            New_Shipping_State = Shipping_State#shipping_state{ship_locations = Ship_Locations_update},
                            {ok, New_Shipping_State};
                        _ -> error
                    end
            end
    end.




%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
