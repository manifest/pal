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

-module(pal).

%% API
-export([
	init/1,
	init/2,
	new/1,
	new/2,
	new/3,
	authenticate/2
]).

%% Definitions
-define(CONTENT_TYPE, <<"content-type">>).

%% Types
-type response()           :: pal_workflow:response().
-type failure_callback()   :: fun((pal_workflow:failure_reason(), cowboy_req:req()) -> {response(), cowboy_req:req()}).
-type success_callback()   :: fun((map(), cowboy_req:req()) -> {response(), cowboy_req:req()}).
-type undefined_callback() :: fun((cowboy_req:req()) -> {response(), cowboy_req:req()}).

-type options()            :: pal_workflow:options().
-type handler_fun()        :: fun((Input :: map(), Req :: cowboy_req:req()) -> {Resp :: response(), Req :: cowboy_req:req()}).
-type handler()            :: pal_workflow:handler(any()) | handler_fun().
-type handler_tree()       :: [handler() | handler_tree()].
-type handler_desc()       :: module() | handler_fun().
-type handler_desc_tree()  :: [handler_desc() | handler_desc_tree()].

-type desc() :: {Input :: map(), Opts :: options()}
						  | {Input :: map(), Workflow :: handler_desc_tree()}
						  | {Input :: map(), Workflow :: handler_desc_tree(), Opts :: options()}.

-record(gwf, {
	input       :: map(),
	handlers    :: handler_tree(),
	onfailure   :: failure_callback(),
	onsuccess   :: success_callback(),
	onundefined :: undefined_callback()
}).

-type workflow() :: #gwf{}.

-export_type([workflow/0, response/0]).

%% ==================================================================
%% API
%% ==================================================================

-spec init([desc()]) -> [workflow()].
init(Descs) ->
	init(Descs, []).

-spec init(desc() | [desc()], options()) -> workflow() | [workflow()].
init({Input, Ds}, GlobalOpts) ->
	init({Input, Ds, []}, GlobalOpts);
init({Input, Ds, Opts}, GlobalOpts) ->
	Opts2 = pt_mlist:merge(GlobalOpts, Opts),
	new(Input, Ds, Opts2);
init(Descs, GlobalOpts) ->
	lists:map(fun(Desc) -> init(Desc, GlobalOpts) end, Descs).

-spec new(handler_desc_tree()) -> workflow().
new(Ds) ->
	new(Ds, []).

-spec new(handler_desc_tree(), options()) -> workflow().
new(Ds, Opts) ->
	new(#{}, Ds, Opts).

-spec new(map(), handler_desc_tree(), options()) -> workflow().
new(Input, Ds, Opts) ->
	Hs =
		pt_list:deepmap(
			fun
				(Fun) when is_function(Fun) ->
					Fun;
				(Mod) ->
					Mod:init(Opts)
			end,
			Ds),

	#gwf{
		input = Input,
		handlers = Hs,
		onfailure = pt_mlist:get(onfailure, Opts, fun onfailure/2),
		onsuccess = pt_mlist:get(onsuccess, Opts, fun onsuccess/2),
		onundefined = pt_mlist:get(onundefined, Opts, fun onundefined/1)}.

-spec authenticate(Req, G | [G]) -> {Resp, Req} when Resp :: response(), Req :: cowboy_req:req(), G :: workflow().
authenticate(Req, Gs) ->
	{RespReq, G} = authenticate_dirty(Req, Gs),
	case G of
		undefined -> normalize(RespReq, new([]));
		_         -> normalize(RespReq, G)
	end.

%% ==================================================================
%% Internal functions
%% ==================================================================

-spec authenticate_dirty(Req, G | [G]) -> {{Resp, Req}, undefined | G} when Resp :: response(), Req :: cowboy_req:req(), G :: workflow().
authenticate_dirty(Req, []) ->
	{{undefined, Req}, undefined};
authenticate_dirty(Req, [G|Gs]) ->
	case authenticate_dirty(Req, G) of
		{{undefined, Req2}, _} ->
			authenticate_dirty(Req2, Gs);
		RespReqG ->
			RespReqG
	end;
authenticate_dirty(Req, #gwf{input = Input, handlers = Hs} = G) ->
	RespReq = execute_and(Input, Req, Hs),
	{RespReq, G}.

-spec execute(map(), Req, H) -> {response(), Req} when Req :: cowboy_req:req(), H :: handler().
execute(Input, Req, H) ->
	case H of
		Fun when is_function(Fun) ->
			Fun(Input, Req);
		{Mod, W} ->
			Mod:execute(Input, Req, W)
	end.

-spec execute_or(map(), Req, HT) -> {response(), Req} when Req :: cowboy_req:req(), HT :: handler_tree().
execute_or(_, Req, []) ->
	{undefined, Req};
execute_or(M, Req, [H|T]) ->
	case execute_and(M, Req, H) of
		{undefined, Req2} ->
			execute_or(M, Req2, T);
		MReq ->
			MReq
	end;
execute_or(M, Req, H) ->
	execute(M, Req, H).

-spec execute_and(map(), Req, HT) -> {response(), Req} when Req :: cowboy_req:req(), HT :: handler_tree().
execute_and(M, Req, []) ->
	{M, Req};
execute_and(M, Req, [H|T]) ->
	case execute_or(M, Req, H) of
		{M2, Req2} when is_map(M2) ->
			execute_and(M2, Req2, T);
		MReq ->
			MReq
	end;
execute_and(M, Req, H) ->
	execute(M, Req, H).

-spec normalize({response(), Req}, G) -> {response(), Req} when Req :: cowboy_req:req(), G :: workflow().
normalize({M, Req}, #gwf{onsuccess = OnSuccess}) when is_map(M) ->
	OnSuccess(M, Req);
normalize({{fail, Reason}, Req}, #gwf{onfailure = OnFailure}) ->
	OnFailure(Reason, Req);
normalize({undefined, Req}, #gwf{onundefined = OnUndefined}) ->
	OnUndefined(Req);
normalize(RespReq, _) ->
	RespReq.

-spec onfailure(pal_workflow:failure_reason(), Req) -> {response(), Req} when Req :: cowboy_req:req().
onfailure(Reason, Req) ->
	Req2 =
		cowboy_req:reply(
			422,
			[{?CONTENT_TYPE, <<"application/json">>}],
			pal_workflow:failure_to_json(Reason),
			Req),

	{stop, Req2}.

-spec onsuccess(map(), Req) -> {response(), Req} when Req :: cowboy_req:req().
onsuccess(M, Req) ->
	{M, Req}.

-spec onundefined(Req) -> {response(), Req} when Req :: cowboy_req:req().
onundefined(Req) ->
	M = #{},
	{M, Req}.

%% ==================================================================
%% Tests
%% ==================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(M, #{}).
-define(REQ, req).
-define(FAIL, {fail, <<>>}).
-define(FC, fun(M, Req) -> {M#{a => pt_map:get(a, M, 0) + 1}, Req} end).
-define(FU, fun(_, Req) -> {undefined, Req} end).
-define(FS, fun(_, Req) -> {stop, Req} end).
-define(FF, fun(_, Req) -> {?FAIL, Req} end).

execute_or_test_() ->
	Test =
		[	{"ok any",          [?FC, ?FC, ?FC], {#{a => 1}, ?REQ}},
			{"undefined front", [?FU, ?FC, ?FC], {#{a => 1}, ?REQ}},
			{"undefined back",  [?FC, ?FC, ?FU], {#{a => 1}, ?REQ}},
			{"stop front",      [?FS, ?FC, ?FC], {stop, ?REQ}},
			{"stop back",       [?FC, ?FC, ?FS], {#{a => 1}, ?REQ}},
			{"fail front",      [?FF, ?FC, ?FC], {?FAIL, ?REQ}},
			{"fail back",       [?FC, ?FC, ?FF], {#{a => 1}, ?REQ}} ],

	[{Desc, ?_assertEqual(Output, execute_or(?M, ?REQ, Input))} || {Desc, Input, Output} <- Test].

execute_and_test_() ->
	Test =
		[	{"ok all",          [?FC, ?FC, ?FC], {#{a => 3}, ?REQ}},
			{"undefined front", [?FU, ?FC, ?FC], {undefined, ?REQ}},
			{"undefined back",  [?FC, ?FC, ?FU], {undefined, ?REQ}},
			{"stop front",      [?FS, ?FC, ?FC], {stop, ?REQ}},
			{"stop back",       [?FC, ?FC, ?FS], {stop, ?REQ}},
			{"fail front",      [?FF, ?FC, ?FC], {?FAIL, ?REQ}},
			{"fail back",       [?FC, ?FC, ?FF], {?FAIL, ?REQ}} ],

	[{Desc, ?_assertEqual(Output, execute_and(?M, ?REQ, Input))} || {Desc, Input, Output} <- Test].

execute_nested_test_() ->
	Test =
		[	{"ok",               [?FU, [?FC, [?FC, ?FC], ?FC], ?FC], {#{a => 3}, ?REQ}},
			{"undefined middle", [?FU, [?FC, [?FU], ?FC], ?FC],      {#{a => 1}, ?REQ}},
			{"stop      middle", [?FU, [?FC, [?FS], ?FC], ?FC],      {stop, ?REQ}},
			{"fail      middle", [?FU, [?FC, [?FF], ?FC], ?FC],      {?FAIL, ?REQ}} ],

	[{Desc, ?_assertEqual(Output, execute_or(?M, ?REQ, Input))} || {Desc, Input, Output} <- Test].

-endif.

