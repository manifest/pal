%% ------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ------------------------------------------------------------------

-module(pal_authentication).
-behaviour(pal_workflow).

%% API
-export([
	raw_info/1,
	handler/1,
	handler_state/1,
	update_handler_state/2
]).

%% Handler callbacks
-export([
	init/1,
	execute/3
]).

%% Definitions
-define(DEFAULT_INCLUDES, [credentials, info, extra, rules]).

%% Types
-record(wf, {
	raw_info = #{} :: map(),
	includes       :: [atom()],
	handler        :: pal_workflow:handler(any())
}).

-type workflow() :: #wf{}.

-export_type([workflow/0]).

%% Callbacks
-callback init(Initializer) -> Handler
	when
		Initializer :: pal_workflow:initializer(),
		Handler     :: pal_workflow:handler(workflow()).

-callback authenticate(AuthM, Req, Workflow) -> {Resp, Req}
	when
		AuthM       :: map(),
		Workflow    :: workflow(),
		Req         :: cowboy_req:req(),
		Resp        :: pal_workflow:response().

%% Optional.
%%
%%	-callback credentials(workflow()) -> map().

%% Optional.
%%
%%	-callback uid(workflow()) -> binary().

%% Optional.
%%
%%	-callback info(workflow()) -> map().

%% Optional.
%%
%%	-callback extra(workflow()) -> map().

%% Optional.
%%
%%	-callback rules(workflow()) -> map().

%% ==================================================================
%% API
%% ==================================================================

-spec raw_info(workflow()) -> map().
raw_info(#wf{raw_info = RawInfo}) ->
	RawInfo.

-spec handler(workflow()) -> pal_workflow:handler(any()).
handler(#wf{handler = H}) ->
	H.

-spec handler_state(workflow()) -> any().
handler_state(#wf{handler = {_, HS}}) ->
	HS.

-spec update_handler_state(fun((any()) -> any()), workflow()) -> workflow().
update_handler_state(Fun, #wf{handler = {HMod, HS}} = W) ->
	W#wf{handler = {HMod, Fun(HS)}}.

%% ==================================================================
%% Handler callbacks
%% ==================================================================

-spec init({pal_workflow:handler(any()), pal_workflow:options()}) -> pal_workflow:handler(workflow()).
init({Handler, Opts}) ->
	Workflow =
		#wf{
			includes = pt_kvterm:get(includes, Opts, ?DEFAULT_INCLUDES),
			handler = Handler},
	
	{?MODULE, Workflow}.

-spec execute(map(), Req, W) -> {Resp, Req} when Resp :: pal_workflow:response(), Req :: cowboy_req:req(), W :: workflow().
execute(AuthM, Req, #wf{handler = {HMod, _}} = W) ->
	case HMod:authenticate(AuthM, Req, W) of
		{RawInfo, Req2} when is_map(RawInfo) ->
			AuthM2 = prepare(AuthM, W#wf{raw_info = RawInfo}),
			{AuthM2, Req2};
		RespReq ->
			RespReq
	end.

%% ==================================================================
%% Internal functions
%% ==================================================================

-spec prepare(map(), workflow()) -> map().
prepare(AuthM, W) ->
	Put =
		fun(Key, Map) ->
			case call(Key, W) of
				undefined -> Map;
				Val       -> pt_map:put(Key, Val, Map)
			end
		end,

	Merge =
		fun(Key, Map) ->
			case call(Key, W) of
				undefined ->
					Map;
				Val ->
					CurrVal = pt_map:get(Key, Map, #{}),
					pt_map:put(Key, maps:merge(CurrVal, Val), Map)
			end
		end,

	MergeWhenIncluded =
		fun(Key, Map) ->
			case lists:member(Key, W#wf.includes) of
				true  -> Merge(Key, Map);
				_     -> Map
			end
		end,

	Put(uid, lists:foldl(MergeWhenIncluded, AuthM, ?DEFAULT_INCLUDES)).

-spec call(atom(), workflow()) -> undefined | any().
call(Function, #wf{handler = {HMod, _}} = W) ->
	case erlang:function_exported(HMod, Function, 1) of
		true ->
			HMod:Function(W);
		false ->
			undefined
	end.

