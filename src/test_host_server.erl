%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 24 Sep 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(test_host_server).

%% API
-export([start/0]).

%%%
-define(AddTestVm,add_test@c50).
-define(AddTestApp,add_test).
-define(AddTestFileName,"add_test.application").
-define(AddTestRepoDir,"add_test" ).
-define(CatalogDir,"application_specs").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=test1(),

    loop(),
    ok.


loop()->
    timer:sleep(20*1000),
    loop().

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    {ok,SpecFiles}=host_server:all_filenames(),
    ["c200.host","c201.host","c202.host","c230.host","c50.host"]=lists:sort(SpecFiles),
    ['ctrl@c200','ctrl@c201','ctrl@c202','ctrl@c230','ctrl@c50']=lists:sort(host_server:get_host_nodes()),
    
    [
     {app1,[{value1,v11},{value2,12}]},
     {app2,[{value1,v21},{value2,22}]},
     {conbee,[{conbee_addr,"172.17.0.2"},
	      {conbee_port,80},
	      {conbee_key,"Glurk"}]}
    ]=host_server:get_application_config(),

    %% Update

    
    
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
