%% ------------------------------------------------------------------
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
%% ------------------------------------------------------------------

-module(pal_http).

%% API
-export([
	response/1,
	response/2,
	response/3,
	reply/2
]).

%% Types
-type status()     :: non_neg_integer().
-type headers()    :: [{binary(), iodata()}].

-record(resp, {
	status         :: status(),
	headers = []   :: headers(),
	body    = <<>> :: iodata()
}).

-type response() :: #resp{}.
-type request()  :: any().

-export_type([status/0, headers/0, response/0, request/0]).

%% ==================================================================
%% API
%% ==================================================================

-spec response(status()) -> response().
response(Status) ->
	#resp{status = Status}.

-spec response(status(), headers()) -> response().
response(Status, Headers) ->
	#resp{status = Status, headers = Headers}.

-spec response(status(), headers(), iodata()) -> response().
response(Status, Headers, Body) ->
	#resp{status = Status, headers = Headers, body = Body}.

-spec reply(response(), fun((status(), headers(), iodata()) -> any())) -> any().
reply(#resp{status = Status, headers = Headers, body = Body}, Fun) ->
	Fun(Status, Headers, Body).

