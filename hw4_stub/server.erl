-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    case maps:get(ChatName, State#serv_st.chatrooms, undefined) of
	undefined ->
	    ChatPID = spawn(chatroom, start_chatroom, [ChatName]),
	    case maps:get(ClientPID, State#serv_st.nicks, undefined) of
			undefined -> error;
			ClientNick ->
				NewState = State#serv_st{chatrooms = maps:put(ChatName, ChatPID, State#serv_st.chatrooms), registrations = maps:put(ChatName, [], State#serv_st.registrations)},
				ChatPID ! {self(), Ref, register, ClientPID, ClientNick},
				NewState
	    end;
	ChatPID ->
		case maps:get(ClientPID, State#serv_st.nicks, undefined) of
			undefined -> error;
			ClientNick ->
				NewState = State#serv_st{registrations=maps:put(ChatName, maps:get(ChatName, State#serv_st.registrations) ++ [ClientPID], State#serv_st.registrations)},
				ChatPID ! {self(), Ref, register, ClientPID, ClientNick},
				NewState
		end
	end.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->image.png
    case maps:get(ChatName, State#serv_st.chatrooms, undefined) of
		undefined -> error;
		ChatPID ->
			NewState = State#serv_st{registrations= maps:update(ChatName, lists:delete(ClientPID, maps:get(ChatName, State#serv_st.registrations)), State#serv_st.registrations)},
			ChatPID ! {self(), Ref, unregister, ClientPID},
			ClientPID ! {self(), Ref, ack_leave},
			NewState
	end.	

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    case maps:values(State#serv_st.nicks) of
		Nicks -> case lists:filter(fun (Nick) -> 
										Nick == NewNick 
									end, Nicks) of
					[] -> 
						maps:foreach(fun (ChatNick, ChatPID) ->
										case maps:get(ChatNick, State#serv_st.registrations, undefined) of
											undefined -> ok;
											ClientPIDs -> case lists:filter(fun (CPID) -> 
																				CPID == ClientPID
																			end, ClientPIDs) of
															[] -> ok;
															_ -> ChatPID ! {self, Ref, update_nick, ClientPID, NewNick}
															end
										end
									end, State#serv_st.chatrooms),
						%%io:format("New nick: ~p~n", [NewNick]),
						ClientPID ! {self(), Ref, ok_nick},
						NewState = State#serv_st{nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks)},
						NewState;
					_ -> 
						%%io:format("Same nick: ~p~n", [NewNick]),
						ClientPID ! {self(), Ref, err_nick_used},
						State
				end
	end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    NewNicks = maps:remove(ClientPID, State#serv_st.nicks),
	maps:foreach(fun (ChatName, ChatPID) ->
					case maps:get(ChatName, State#serv_st.registrations, undefined) of
						undefined -> ok;
						ClientPIDs -> case lists:filter(fun (CPID) -> 
															CPID == ClientPID
														end, ClientPIDs) of
										[] -> ok;
										_ -> ChatPID ! {self(), Ref, unregister, ClientPID}
									end
					end
				end, State#serv_st.chatrooms),
	NewRegistrations = maps:map(fun (_ChatName, CPIDs) ->
									lists:delete(ClientPID, CPIDs)
								end, State#serv_st.registrations),
	ClientPID ! {self(), Ref, ack_quit},
	NewState = State#serv_st{nicks = NewNicks, registrations = NewRegistrations},
	NewState.
