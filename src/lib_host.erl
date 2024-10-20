%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_host).
  
-include("host.hrl").

 
%% API
-export([
	 all_filenames/1,
	 get_host_nodes/1

	]).



-export([

	 get_application_config/1
	]).


-export([
	 all_hosts/1,
	 get/3,
	 init/2,
	 update/2,
	 timer_to_call_update/1
	]).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
all_filenames(SpecsDir)->
    {ok,Files}=file:list_dir(SpecsDir),
    SpecFiles=[File||File<-Files,
		     ?SpecExt=:=filename:extension(File)],
    {ok,SpecFiles}.
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_host_nodes(SpecsDir)->
    Result=case all_filenames(SpecsDir) of
	       {ok,SpecFiles}->
		   get_host_nodes(SpecFiles,SpecsDir,[]);
	       Error ->
		    Error
	   end,   
    Result.
	
get_host_nodes([],_,Acc)->
    Acc;
get_host_nodes([File|T],SpecsDir,Acc)->
    {ok,[Info]}=file:consult(filename:join(SpecsDir,File)),
    HostNode=maps:get(host_node,Info),
    get_host_nodes(T,SpecsDir,[HostNode|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_application_config(SpecsDir)->
    {ok,MyHostName}=net:gethostname(),
    Result=case  all_filenames(SpecsDir) of
	       {ok,SpecFiles}->
		   get_application_config(SpecFiles,MyHostName,SpecsDir,[],false);
	       Error ->
		    Error
	   end,   
    Result.
	

get_application_config([],MyHostName,SpecsDir,_,false)->
    {error,["Specification for host doesnt exist in the reps ",MyHostName,SpecsDir]};
get_application_config(_,_,_,ApplicationConfig,true)->
    {ok,ApplicationConfig};
get_application_config([File|T],MyHostName,SpecsDir,ApplicationConfig,false)->
    {ok,[Info]}=file:consult(filename:join(SpecsDir,File)),
    HostName=maps:get(hostname,Info),
    if 
	HostName=:=MyHostName->
	    NewApplicationConfig=maps:get(application_config,Info),
	    Found=true;
	true->
	    NewApplicationConfig=ApplicationConfig,
	    Found=false
    end,
    get_application_config(T,MyHostName,SpecsDir,NewApplicationConfig,Found).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
all_hosts(RepoDir)->
    Result=case git_handler:all_filenames(RepoDir) of
	       {ok,AllFileNames}->
		   get_hostnames(AllFileNames,RepoDir,[]);
	       Error ->
		    Error
	   end,   
    Result.
	
get_hostnames([],_,Acc)->
    Acc;
get_hostnames([FileName|T],RepoDir,Acc)->
    {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
    HostName=maps:get(hostname,Info),
    get_hostnames(T,RepoDir,[HostName|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get(Type,FileName,RepoDir)->
    Result=case git_handler:read_file(RepoDir,FileName) of
	       {ok,[Info]}->
		   {ok,maps:get(Type,Info)};
	       Error ->
		    Error
	   end,   
    Result.
	

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
timer_to_call_update(Interval)->
    timer:sleep(Interval),
    rpc:cast(node(),host_server,check_update_repo,[]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update(RepoDir,GitPath)->
    Result=case git_handler:is_repo_updated(RepoDir) of
	       {error,["RepoDir doesnt exists, need to clone"]}->
		   ok=git_handler:clone(RepoDir,GitPath),
		   {ok,"Cloned the repo"};
	       false ->
		   ok=git_handler:update_repo(RepoDir),
		   {ok,"Pulled a new update of the repo"};
	       true ->
		   {ok,"Repo is up to date"}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
init(RepoDir,GitPath)->
    case git_handler:is_repo_updated(RepoDir) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    ok=git_handler:clone(RepoDir,GitPath);
	{error,"fatal: not a git repository (or any of the parent directories): .git,Not up to date"}->
	    file:del_dir_r(RepoDir),
	      ok=git_handler:clone(RepoDir,GitPath);
	false ->
	    ok=git_handler:update_repo(RepoDir);
	true ->
	    ok
    end,
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
