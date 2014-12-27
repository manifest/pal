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

-module(pal_workflow).

%% API
-export([
	from_json/2,
	failure_to_json/1
]).

%% Types
-type options()        :: pt_kvterm:kvterm().
-type failure_reason() :: binary() | [{binary(), binary()}] | map().
-type response()       :: stop | {fail, failure_reason()} | undefined | map().
-type handler(W)       :: {Mod :: module(), W}.
-type initializer()    :: {module(), options()} | options().

-export_type([handler/1, response/0, failure_reason/0, options/0]).

%% Callbacks
-callback init(Initializer) -> Handler
	when
		Initializer :: initializer(),
		Handler     :: handler(any()).

-callback execute(Input, Req, W) -> {Resp, Req}
	when
		Input       :: map(),
		W           :: any(),
		Req         :: cowboy_req:req(),
		Resp        :: response().

%% ==================================================================
%% API
%% ==================================================================

-spec failure_to_json(failure_reason()) -> binary().
failure_to_json(B) when is_binary(B) ->
  jsx:encode([{<<"error">>, B}]);
failure_to_json(M) when is_map(M) ->
  jsxn:encode(M);
failure_to_json(L) ->
  jsx:encode(L).

-spec from_json(binary(), fun((map()) -> response())) -> response().
from_json(Input, When) ->
	try jsxn:decode(Input) of
		Encoded ->
			When(Encoded)
	catch
		_:_ ->
			{fail, <<"Bad formatted JSON.">>}
	end.

