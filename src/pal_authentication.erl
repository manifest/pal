%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014-2015 Andrei Nesterov <ae.nesterov@gmail.com>
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
%% ----------------------------------------------------------------------------

-module(pal_authentication).
-behaviour(pal_workflow).

%% Workflow callbacks
-export([
	decl/0,
	handle/4
]).

%% Definition
-define(INCLUDES, [uid, credentials, info, extra, rules]).

%% Types
-type rawdata() :: [{binary(), binary() | true}].
-type result()
	:: {ok,    Data     :: rawdata()}
	 | {stop,  HttpResp :: pal_http:response()}
	 | {error, Reason   :: pal_workflow:error_reason()}.

-export_type([result/0, rawdata/0]).

%% Callbacks
-callback authenticate(Handlers, Data, Metadata, State) -> Result
	when
		Handlers :: list(module()),
		Data     :: map(),
		Metadata :: map(),
		State    :: map(),
		Result   :: pal_authentication:result().

%% Optional.
%%
%%	-callback uid(RawData :: rawdata()) -> binary().

%% Optional.
%%
%%	-callback credentials(RawData :: rawdata(), Acc :: map()) -> map().

%% Optional.
%%
%%	-callback info(RawData :: rawdata(), Acc :: map()) -> map().

%% Optional.
%%
%%	-callback extra(RawData :: rawdata(), Acc :: map()) -> map().

%% Optional.
%%
%%	-callback rules(RawData :: rawdata(), Acc :: map()) -> map().

%% ============================================================================
%% Workflow callbacks
%% ============================================================================

-spec decl() -> pal_workflow:declaration().
decl() ->
	Opts =
		#{includes => ?INCLUDES},

	{?MODULE, Opts}.

-spec handle(list(module()), map(), map(), map()) -> pal_workflow:result().
handle([_|T], Data, Meta, State) ->
	case pt_modlist:callr_sublist(T, authenticate, [Data, Meta, State]) of
		{ok, RawData} ->
			Includes = pt_list:with(?INCLUDES, maps:get(includes, State, [])),
			{ok, build(Includes, T, RawData, Data)};
		Result ->
			Result
	end.

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec build(list(atom()), list(module()), rawdata(), map()) -> map().
build([uid|T], Hs, Data, M) ->
	case pt_modlist:callr(Hs, uid, [Data], error) of
		error -> build(T, Hs, Data, M);
		Val   -> build(T, Hs, Data, maps:put(uid, Val, M))
	end;
build([credentials|T], Hs, Data, M) ->
	build(T, Hs, Data, pt_modlist:callr(Hs, credentials, [Data, M], M));
build([H|T], Hs, Data, M) ->
	Acc = maps:get(H, M, #{}),
	case pt_modlist:callr(Hs, H, [Data, Acc], error) of
		error -> build(T, Hs, Data, M);
		Acc2  -> build(T, Hs, Data, maps:put(H, Acc2, M))
	end;
build([], _, _, M) -> M.

