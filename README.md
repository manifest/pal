# Pragmatic Authentication Library

[![Build Status][travis_img]][travis]

standardized multi-provider authentication library and the best pal of Cowboy

### Introduction

PAL is designed to minimize the effort of adding a user authentication
for web applications written in Erlang using [Cowboy][cowboy] HTTP server.
Any developer can extend the library by creating their own authentication workflow for everything from Facebook to LDAP.
PAL is inspired by [OmniAuth][omniauth], [Friend][friend] and [Passport][passport].

### Hot to use

A simple example for those who prefer to study the code rather than documentation.  
Complete working example can be found [here][pal-example].

```erlang
W = pal:new([Workflow]),
case pal:authenticate(Req, W) of
	{#{uid := _} = M, Req2} ->
		io:format("User has been authorized:~n~p~n", [M]);
	{#{credentials := _} = M, Req2} ->
		io:format("User has been authenticated but not authorized:~n~p~n", [M]);
	{#{} = M, Req2} ->
		io:format("User hasn't been authenticated:~n~p~n", [M]);
	{halt, Req2} ->
		io:format("Error has occured or workflow must return an HTTP response as a part of normal execution")
end.

%%	User has been authorized:
%%	#{uid => <<"1234567890">>,
%%	  info =>
%%	    #{first_name => <<"John">>,
%%	      last_name  => <<"Doe">>,
%%	      name       => <<"John Doe">>},
%%	  rules =>
%%	    #{role => user},
%%	  credentials =>
%%	   #{access_token  => <<"token-value">>,
%%	     token_type    => <<"Bearer">>,
%%	     expires_in    => 3600,
%%	     refresh_token => <<"another-token-value">>}}
```

### Overview

#### Workflow

Workflow is a fundamental unit of the library. It may be defined as a function or a module.

- The module can hold a state. And it is a preferred form to share a workflow with the community.
- The function is useful for rapid implementation, debugging, an application-specific logic.

Any workflow consumes a set of parameters presented as an Erlang map `Input`,
an instance of HTTP request `Req`, and optionally, a state `State` in case the workflow is a module.

The result of the workflow execution will be one of the following:

- *success()* :: `{Output, Req}`  
	in case of success, `Output` is an Erlang map and contains a direct result of the workflow execution
- *undeifned()* :: `{undefined, Req}`  
	when not enough data for normal execution and the control should be delegated to an another workflow
- *failure()* :: `{{fail, Reason}, Req}`  
	when an error has occurred, `Reason` will contain the reason
- *halt()* :: `{halt, Req}`  
	when workflow must return an HTTP response as a part of normal execution  
	(for instance, redirect to the login page of OAuth2 provider)

![pal-workflow][pal-workflow-img]

#### Auth Map

The `Input` and the `Output` of workflow represent an authentication map which should have the following structure:

- `provider` (managed by the library) -
		The provider with which the user authenticated (e.g. 'twitter' or 'facebook').
- `uid` -
		An identifier unique to the given provider, such as a Twitter user ID. Should be stored as a string.
- `info` -
		A map containing information about the user:
	- `name` -
			The best display name known to the workflow.
			Usually a concatenation of first and last name, but may also be an arbitrary designator or nickname for some workflows.
	- `email` -
			The e-mail of the authenticating user.
			Should be provided if at all possible (but some sites such as Twitter do not provide this information).
	- `nickname` -
			The username of an authenticating user (such as your @-name from Twitter or GitHub account name).
	- `first_name`
	- `last_name`
	- `location` -
			The general location of the user, usually a city and state.
	- `description` -
			A short description of the authenticating user.
	- `image` -
			A URL representing a profile image of the authenticating user.
			Where possible, should be specified to a square, roughly 50x50 pixel image.
	- `phone` -
			The telephone number of the authenticating user (no formatting is enforced).
	- `urls` -
			A map containing key value pairs of an identifier for the website and its URL.
			For instance, an entry could be `#{<<"github">> => <<"https://github.com/manifest/pal">>}`.
- `extra` -
		Contains extra information returned from the authentication provider.
		May be in provider-specific formats.
	- `raw_info` -
			A map of all information gathered about a user in the format it was gathered.
			For example, for Twitter users this is a hash representing the JSON hash returned from the Twitter API.
- `credentials`
		If the authenticating service provides some kind of access token
		or other credentials upon authentication, these are passed through here.
		Follow to the protocol specification in naming of keys (For instance, in case of oauth2 protocol:
		`access_token`, `token_type`, `expires_in`, etc. according to [RFC 6749][rfc6749-credentials])
- `rules` -
		Any rules, like ACL or user roles.
		For instance, a user could have 'admin' role or read/write access to some files.

All keys are optional, but it is important to follow structure of the authentication map always when possible.

#### Sequence of workflows

Workflows can be connected in a sequence.
The process of delegating control between workflows may differ depending on the type of sequence:

- In case of and-sequence, the control will be delegated to the next workflow only if previous one is completed successfully, otherwise the execution of the sequence will be stopped.

![pal-sequence-end][pal-workflow-sequence-and-img]

- In case of or-sequence, the control will be delegated to the next workflow if previous one returns *undefined()*. The sequence will be stopped if any of workflow returns *halt()* or *failure()*

![pal-sequence-or][pal-workflow-sequence-or-img]

- and-sequence and or-sequences may be combined

![pal-sequence-or-and][pal-workflow-sequence-or-and-img]

#### Main workflow and the group of sequences

On the top level (the level on which a user of the library operates)
the sequences of workflows are combined into the main workflow (or into a group of sequences, in another words).
The result obtained from the sequence of workflows is passed through one of callback-function
and its value is reduced to one of two possible (by default, user of library can change this behaviour):

- *success()* :: `{Output, Req}`  
	if the result obtained from the sequence of workflows is *undefined()*
	the `Output` will contain an empty Erlang map
- *halt()* :: `{halt, Req}`  
	if the result obtained from the sequence of workflows is *failure()*
	the HTTP response with the status code equals 422 and the body containing a reason of failure in json format
	will be generated

![pal-workflow-group][pal-workflow-group-img]

It is possible to create more than one group of sequences in the main workflow.
In this case the control of execution is delegated to the next group on the basis of or-sequence.
To be able to determine the execution of the which group is completed successfully,
the user sets a provider value for each group. That value will be added to the final authentication map.

#### Examples of creating the main workflow

```erlang
%% minimal example
pal:new([workflow(1)]).
pal:new([workflow(1)], Opts).

%% and-sequence: workflow(1) and workflow(2)
pal:new([workflow(1), workflow(2)]).

%% or-sequence: workflow(1) or workflow(2)
pal:new([[workflow(1), workflow(2)]]).

%% or-and-sequence: (workflow(1) or workflow(2)) and workflow(3)
pal:new([[workflow(1), workflow(2)], workflow(3)]).

%% named groups
pal:init([{"example", pal_workflow}]).
pal:init([{"example", [workflow(1), workflow(2)], Opts}]).
pal:init([
		{"example-1", [workflow(1), workflow(2)], LocalOpts1},
		{"example-2", [workflow(3)], LocalOpts2}
	], GlobalOpts).
```

### List of workflows

Provider | Workflow                                              | Description
---------|-------------------------------------------------------|----------------
Google   | [`pal_google_oauth2_authcode`][pal-google-oauth2]     | Google Login (OAuth2 Authorization Code Grant)
Google   | [`pal_google_oauth2_tokeninfo`][pal-google-oauth2]    | Validating an ID token
Google   | [`pal_google_oauth2_people`][pal-google-oauth2]       | Obtaining user profile information
Facebook | [`pal_facebook_oauth2_authcode`][pal-facebook-oauth2] | Facebook Login (OAuth2 Authorization Code Grant)
Facebook | [`pal_facebook_oauth2_user`][pal-facebook-oauth2]     | Obtaining user profile information
OAuth2   | [`pal_oauth2_authcode`][pal-oauth2]                   | OAuth2 Authorization Code Grant, [RFC 6749][rfc6749]
Basic    | [`pal_basic`][pal-basic]                              | HTTP Basic Access Authentication, [RFC 2617][rfc2617]
Initial  | [`pal_authentication`][pal]                           | Initial and the simplest behaviour of any workflow

### License

Provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/pal
[travis_img]:https://secure.travis-ci.org/manifest/pal.png
[cowboy]:https://github.com/extend/cowboy
[omniauth]:https://github.com/intridea/omniauth
[friend]:https://github.com/cemerick/friend
[passport]:https://github.com/jaredhanson/passport
[rfc2617]:https://tools.ietf.org/html/rfc2617
[rfc6749]:http://tools.ietf.org/html/rfc6749
[rfc6749-credentials]:http://tools.ietf.org/html/rfc6749#section-4.2.2
[pal-workflow-img]:misc/pal-workflow.png
[pal-workflow-sequence-and-img]:misc/pal-workflow-sequence-and.png
[pal-workflow-sequence-or-img]:misc/pal-workflow-sequence-or.png
[pal-workflow-sequence-or-and-img]:misc/pal-workflow-sequence-or-and.png
[pal-workflow-group-img]:misc/pal-workflow-group.png
[pal]:https://github.com/manifest/pal
[pal-basic]:https://github.com/manifest/pal-basic
[pal-oauth2]:https://github.com/manifest/pal_oauth2.git
[pal-google-oauth2]:https://github.com/manifest/pal_google_oauth2.git
[pal-facebook-oauth2]:https://github.com/manifest/pal_facebook_oauth2.git
[pal-example]:https://github.com/manifest/pal-example

