%%%
%%%  Copyright � IDEALX S.A.S. 2003
%%%
%%%	 Author : Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
%%%  Created: 22 Dec 2003 by Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%% 
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%%-------------------------------------------------------------------
%%% File    : ts_client_proxy.erl
%%% Author  : Nicolas Niclausse <nniclausse@idealx.com>
%%% Description : handle communication with client and server.
%%%
%%% Created : 22 Dec 2003 by Nicolas Niclausse <nniclausse@idealx.com>
%%%-------------------------------------------------------------------


-module(ts_client_proxy).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ts_profile.hrl").
-include("ts_http.hrl").

-define(lifetime, 120000).
-define(tcp_buffer, 65536).

%%--------------------------------------------------------------------
%% External exports
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		 code_change/3]).

-record(state, {
          clientsock,
          http_version,
          parse_status   = new, %% http status = body|new
          body_size      = 0,
          content_length = 0,
          buffer = [],
          serversock
          }).
%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the gen_server with the socket given by the listener
%%--------------------------------------------------------------------
start(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Socket]) ->
    {ok, #state{clientsock=Socket}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
% client data, parse and send it to the server.
handle_info({tcp, ClientSock, String}, State) 
  when ClientSock == State#state.clientsock ->
    ts_utils:inet_setopts(tcp, ClientSock,[{active, once}]),
    {ok, NewState}  = parse(State,ClientSock,State#state.serversock,String),
    {noreply, NewState, ?lifetime};

% server data, send it to the client
handle_info({Type, ServerSock, String}, State) 
  when ServerSock == State#state.serversock, ((Type == tcp) or (Type == ssl)) ->
    ts_utils:inet_setopts(Type, ServerSock,[{active, once}]),
    ?LOGF("Received data from server: ~s~n",[String],?DEB),
    {ok,NewString} = ts_utils:from_https(String),
    send(State#state.clientsock, NewString),
    {noreply, State, ?lifetime};

%%%%%%%%%%%% Errors and termination %%%%%%%%%%%%%%%%%%%

% Log who did close the connection, and exit.
handle_info({Msg,Socket},State=#state{http_version = HTTPVersion,
                                      serversock = Socket
                                     }) when Msg==tcp_close; Msg==ssl_closed ->
    ?LOG("socket closed by server~n",?INFO),
    case HTTPVersion of
        "HTTP/1.0" ->
            {stop, normal, ?lifetime};%Disconnect client if it requires HTTP/1.0
        _ ->
            {noreply, State#state{serversock=undefined}, ?lifetime}
    end;

handle_info({Msg, _Socket}, State) when Msg == tcp_closed;
                                       Msg == ssl_closed->
    ?LOG("socket closed by client~n",?INFO),
    {stop, normal, State};

% Log properly who caused an error, and exit.
handle_info({Msg, Socket, Reason}, State) when Msg == tcp_error;
                                               Msg == ssl_error ->
    ?LOGF("error on socket ~p ~p~n",[Socket,Reason],?ERR),
    {stop, {error, sockname(Socket,State), Reason}, State};

handle_info(timeout, State) ->
    {stop, timeout, State};

handle_info(_Info, State) ->
    {stop, unknown, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
%    ts_proxy_recorder:dorecord(endsession),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% Function: sockname/2
%%           sockname(Socket,State)
%% Purpose: decides whether some socket is the client or the server
%% Description: State contains two fields, "serversock" and "clientsock".
%%              This function searches Socket among the two, and returns
%%              an appropriate atom among 'server', 'client' and 'unknown'.
%% Returns: Sockname
%% Types:   Sockname -> server | client | unknown
%%          State -> state_record()

sockname(Socket,#state{serversock=Socket})-> server;
sockname(Socket,#state{clientsock=Socket})-> client;
sockname(_Socket,_State)-> unknown.



%%--------------------------------------------------------------------
%% Func: parse/4
%% Purpose: parse HTTP request
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
parse(State=#state{parse_status=Status},_,ServerSocket,String) when Status==new ->
    NewString = lists:append(State#state.buffer,String),
	case ts_http_common:parse_req(NewString) of 
		{more, _Http, _Head} -> 
            ?LOG("Headers incomplete, buffering ~n",?DEB),
			{ok, State#state{parse_status=new, buffer=NewString}}; %FIXME: not optimal
        {ok, Http=#http_request{url=RequestURI, version=HTTPVersion}, Body} -> 
            ?LOGF("Method ~p ~n",[Http#http_request.method],?DEB),
            ?LOGF("Headers ~p ~n",[Http#http_request.headers],?DEB),
            case httpd_util:key1search(Http#http_request.headers,"content-length") of
                undefined -> % no body, everything received
                    ts_proxy_recorder:dorecord({Http }),
                    {NewSocket,RelURL} = check_serversocket(ServerSocket,RequestURI),
                    ?LOGF("Remove server info from url:~p ~p in ~p~n",
                          [RequestURI,RelURL,NewString], ?INFO),
                    {ok, RealString} = relative_url(NewString,RequestURI,RelURL),
                    send(NewSocket,RealString),
                    {ok, State#state{http_version=HTTPVersion,
                                     parse_status = new, buffer=[],
                                     serversock=NewSocket}};
                Length ->
                    CLength = list_to_integer(Length),
                    ?LOGF("HTTP Content-Length:~p~n",[CLength], ?DEB),
                    BodySize = length(Body),
                    if
                        BodySize == CLength ->  % end of response
                            {NewSocket,RelURL} = check_serversocket(ServerSocket,RequestURI),
                            {ok,RealString,_Count} = regexp:gsub(NewString,RequestURI,RelURL),%FIXME: why not use relative_url ?
                            send(NewSocket,RealString),
                            ?LOG("End of response, recording~n", ?DEB),
                            ts_proxy_recorder:dorecord({Http#http_request{body=Body}}),
                            {ok, State#state{http_version = HTTPVersion,
                                             parse_status = new, buffer=[],
                                             serversock=NewSocket}};
                        BodySize > CLength  ->
                            {error, bad_content_length};
                        true ->
                            {NewSocket,RelURL} = check_serversocket(ServerSocket,RequestURI),
                            {ok,RealString,_Count} = regexp:gsub(NewString,RequestURI,RelURL), %FIXME: why not use relative_url ?
                            send(NewSocket,RealString),
                            ?LOG("More data to come continue before recording~n", ?DEB),
                            {ok, State#state{http_version=HTTPVersion,
                                             content_length = CLength,
                                             body_size = BodySize,
                                             serversock=NewSocket,
                                             buffer = Http#http_request{body=Body },
                                             parse_status = body
                                            }
                            }
                    end
            end
    end;

parse(State=#state{parse_status=Status, buffer=Http},_,ServerSocket,String) 
  when Status == body ->
	DataSize = length(String),
	?LOGF("HTTP Body size=~p ~n",[DataSize], ?DEB),
	Size = State#state.body_size + DataSize,
	CLength = State#state.content_length,
    send(ServerSocket, String),
    Buffer=lists:append(Http#http_request.body,String),
    %% Should be checked before
	case Size of 
		CLength -> % end of response
            ?LOG("End of response, recording~n", ?DEB),
            ts_proxy_recorder:dorecord( {Http#http_request{ body=Buffer }} ),
			{ok, State#state{body_size=0,parse_status=new, content_length=0,buffer=[]}};
		_ ->
            ?LOGF("Received ~p bytes of data, wait for ~p, continue~n", [Size,CLength],?DEB),
			{ok, State#state{body_size = Size, buffer = Http#http_request{body=Buffer}}}
	end.

%%--------------------------------------------------------------------
%% Func: check_serversocket/2
%% Purpose: If the socket is not defined, or if the server is not the
%%          same, connect to the server as specified in URL
%% Returns: Socket
%%--------------------------------------------------------------------            
check_serversocket(Socket, "http://{" ++ Rest) ->
    check_serversocket(Socket, ts_config_http:parse_URL("https://"++Rest));
check_serversocket(Socket, URL) when list(URL)->
    check_serversocket(Socket, ts_config_http:parse_URL(URL));
check_serversocket(undefined, URL) ->
    Port = ts_config_http:set_port(URL),
    ?LOGF("Connecting to ~p:~p ...~n", [URL#url.host, Port],?DEB),

    {ok, Socket} = connect(URL#url.scheme, URL#url.host,Port),

    ?LOGF("Connected to server ~p on port ~p (socket is ~p)~n",
          [URL#url.host,Port,Socket],?INFO),
    case URL#url.querypart of 
        []    -> {Socket, URL#url.path};
        Query -> {Socket, URL#url.path++"?"++Query}
    end;
    
check_serversocket(Socket, URL=#url{port=Port,host=Host}) ->
    RealPort = ts_config_http:set_port(URL),
    {ok, RealIP} = inet:getaddr(Host,inet),
    case peername(Socket) of
        {ok, {RealIP, RealPort}} -> % same as previous URL
            ?LOGF("Reuse socket ~p on URL ~p~n", [Socket, URL],?DEB),
            case URL#url.querypart of 
                []    -> {Socket, URL#url.path};
                Query -> {Socket, URL#url.path++"?"++Query}
            end;
        Other ->
            ?LOGF("New server configuration  (~p:~p, was ~p) on URL ~p~n", 
                  [RealIP, RealPort, Other, URL],?DEB),
            case Socket of 
                {sslsocket, _, _} -> ssl:close(Socket);
                _             -> gen_tcp:close(Socket)
            end,
            {ok, NewSocket} = connect(URL#url.scheme, Host,RealPort),
            case URL#url.querypart of 
                []    -> {NewSocket, URL#url.path};
                Query -> {NewSocket, URL#url.path++"?"++Query}
            end
    end.

peername({sslsocket,A,B})-> ssl:peername({sslsocket,A,B});
peername(Socket)         -> prim_inet:peername(Socket).


send({sslsocket,A,B},String) ->
    {ok, RealString } = ts_utils:to_https({request,String}),
    ?LOGF("Sending data to ssl socket ~p ~p (~p)~n", [A, B, RealString],?DEB),
    ssl:send({sslsocket,A,B}, RealString);
send(Socket,String) ->
    gen_tcp:send(Socket,String).

connect(Scheme, Host, Port)->
    case Scheme of 
        https -> 
            {ok, _} = ssl:connect(Host,Port,
                                  [{active, once}]);
        http  -> 
            {ok, _} = gen_tcp:connect(Host,Port,
                                      [{active, once},
                                       {recbuf, ?tcp_buffer},
                                       {sndbuf, ?tcp_buffer}
                                      ])
    end.

relative_url(NewString,RequestURI,RelURL)->
    [FullURL_noargs|_] = string:tokens(RequestURI,"?"),
    [RelURL_noargs|_]  = string:tokens(RelURL,"?"),
    {ok,RealString,_Count} = regexp:gsub(NewString,FullURL_noargs,RelURL_noargs),
    {ok, RealString}.
