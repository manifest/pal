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

-module(pal).

%% API
-export([
	new/2,
	group/2,
	authenticate/2,
	authenticate/3
]).

%% Types
-type workflow() :: pal_workflow:workflow().
-type result()   :: pal_workflow:result().
-type group()    :: list(workflow()).

-export_type([workflow/0, group/0, result/0]).

%% ============================================================================
%% API
%% ============================================================================

-spec new(module(), map()) -> workflow().
new(Mod, Opts) ->
	pt_workflow:init(Mod, Opts).

-spec group(list(module()), map()) -> group().
group(Workflows, Opts) ->
	lists:map(fun(Mod) ->
		pt_workflow:init(Mod, Opts)
	end, Workflows).

-spec authenticate(map(), workflow() | group()) -> result().
authenticate(Data, WG) ->
	authenticate(Data, #{}, WG).

-spec authenticate(map(), map(), workflow() | group()) -> result().
authenticate(Data, Meta, [H|T]) ->
	handle_result(
		pt_workflow:callr_sublist(handle, [Data, Meta], H),
		Meta,
		T);
authenticate(Data, Meta, W) ->
	handle_result(
		pt_workflow:callr_sublist(handle, [Data, Meta], W),
		Meta,
		[]).

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec handle_result(result(), map(), group()) -> result().
handle_result({ok, _} = Ok, _, []) -> Ok;
handle_result({ok, Data}, Meta, L) -> authenticate(Data, Meta, L);
handle_result(Result, _, _)        -> Result.

