# Pragmatic Authentication Library

[![Build Status][travis_img]][travis]

The standardized multi-provider authentication library.

### Introduction

PAL is designed to minimize the efforts of integration a user authentication to the web application.
It can be used with any HTTP server and any developer can extend the library,
create their own authentication workflow for everything from Facebook to LDAP.
PAL is inspired by [OmniAuth][omniauth], [Friend][friend] and [Passport][passport].

#### How to use

If you prefer to study the code rather documentation,
the example below will show how to use the implementation of Google Login.  
You also can find the complete example using PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

At first, you need to create the workflow.
Workflow may have an required and optional options.
We pass them and a name of the module with workflow implementation to the `pal:new/2` function.

```erlang
Options =
  #{client_id     => <<"...">>,
    client_secret => <<"...">>,
    redirect_uri  => <<"https://localhost/...">>}.

Workflow =
  pal:new(
    pal_google_oauth2_authcode,
    Options).
```

When our workflow was created, the half an job had been done.
All we now need, parse the request and pass that data to the `pal:authenticate/2` function.

```erlang
pal:authenticate(Data, Workflow).

%% #{access_token => <<"...">>,
%%   token_type => <<"Bearer">>,
%%   expires_in => 3600,
%%   id_token => <<"...">>,
%%   code => <<"...">>}
```

That's all.

#### How it works

When a user come to us first time, the request doesn't contain any data.
We have to redirect a user to authentication provider and we do it:

```erlang
pal:authenticate(#{}, Workflow).

%% {stop,{resp,303, [{<<"location">>, <<"https://accounts.google.com/...">>}], <<>>}}
```

For Google Login (OAuth2 Authorization Code Grant) workflow
we need to retrieve `code`, `state` and `error` fields from the query string of request
if any of them appears, and pass that data to the `pal:authenticate/2` function.

```erlang
pal:authenticate(#{code => <<"...">>}, Workflow).

%% #{access_token => <<"...">>,
%%   token_type => <<"Bearer">>,
%%   expires_in => 3600,
%%   id_token => <<"...">>,
%%   code => <<"...">>}
```

[Cowboy][cowboy] specific implementation parsing the data of request:

```erlang
Data =
  lists:foldl(
    fun({Key, Val}, M) ->
      maps:put(binary_to_existing_atom(Key, utf8), Val, M)
    end,
    #{},
    pt_kvlist:with(
      [<<"code">>, <<"state">>, <<"error">>],
      cowboy_req:parse_qs(Req))).
```

#### Group

We can combine more than one workflow in the sequence.
Below, we are using the `access_token` (result of the first workflow execution)
to obtain the Google+ profile information.

```erlang
Workflow =
  pal:group(
    [pal_google_oauth2_authcode, pal_google_oauth2_people],
    Options),

pal:authenticate(Data, Workflow).

%% #{uid => <<"...">>,
%%   info =>
%%     #{name => <<"John Doe">>,
%%       first_name => <<"John">>,
%%       last_name => <<"Doe">>,
%%       image => <<"https://lh3.googleusercontent.com/...">>, 
%%       urls => #{
%%         <<"google">> => <<"https://plus.google.com/...">>}},
%%   access_token => <<"...">>,
%%   token_type => <<"Bearer">>,
%%   expires_in => 3600,
%%   id_token => <<"...">>,
%%   code => <<"...">>}
```

### Overview

Workflow is a fundamental unit of the library. It have to be defined as a module
implementing at least the `pal_workflow` behaviour. Note that it is highly recommended to
also implement the `pal_authentication` behaviour because it is responsible for the authentication schema creation flow.

Any workflow has a state. The `pal:init/2` and `pal:group/2` functions are responsible for its initialization.
They expect in the arguments: a name of the module (list of module names in case of group)
with the workflow implementation and an initial options of the workflow.

The workflow can be executed by calling `pal:authenticate/{2,3}` function.
It expects in the arguments: parsed data were received with the request,
optional data from any other source (server-side data) and the previously created state of workflow.

The result would contain data representing the authentication scheme `{ok, NewData}`
or error with the reason `{error, Reason}` or a HTTP response `{stop, HttpResp}`
must be passed back to a user to continue an authentication process.
The error reason is represented by the tuple with the type and the error data
in the workflow specific format (for instance, `{oauth2, #{error => access_denied}}`).

![pal-authenticate][pal-authenticate-img]

#### Authentication Schema

- `uid` -
		An identifier unique to the given provider, such as a Twitter user ID. Should be stored as a binary string.
- **any credentials are passed through here (on the root level)**  
		If the authenticating service provides some kind of access token
		or other credentials upon authentication, these are passed through here.
		Follow to the protocol specification in naming of keys (For instance, for OAuth2:
		`access_token`, `token_type`, `expires_in`, etc. according to [RFC 6749][rfc6749-credentials])
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
			A map containing key-value pairs of an identifier for the website and its URL.
			For instance, an entry could be `#{<<"github">> => <<"https://github.com/manifest/pal">>}`.
- `extra` -
		Contains extra information returned from the authentication provider.
		May be in provider-specific formats.
- `rules` -
		Any rules, like ACL or user roles.
		For instance, a user might have an 'admin' role or read/write access to some files.

All keys of authentication schema are optional, but it is important to follow this structure always when it's possible.

### List of workflows

Provider  | Workflow                                              | Description
----------|-------------------------------------------------------|----------------
Google    | [`pal_google_oauth2_authcode`][pal-google-oauth2]     | Google Login (OAuth2 Authorization Code Grant)
Google    | [`pal_google_oauth2_tokeninfo`][pal-google-oauth2]    | Google OAuth2 token validation
Google    | [`pal_google_oauth2_people`][pal-google-oauth2]       | Google+ profile information
Facebook  | [`pal_facebook_oauth2_authcode`][pal-facebook-oauth2] | Facebook Login (OAuth2 Authorization Code Grant)
Facebook  | [`pal_facebook_oauth2_user`][pal-facebook-oauth2]     | Facebook profile information
OAuth2    | [`pal_oauth2_authcode`][pal-oauth2]                   | OAuth2 Authorization Code Grant, [RFC 6749][rfc6749]
Behaviour | [`pal_authentication`][pal]                           | Behaviour of PAL workflow

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/pal?branch=master
[travis_img]:https://secure.travis-ci.org/manifest/pal.png
[cowboy]:https://github.com/extend/cowboy
[omniauth]:https://github.com/intridea/omniauth
[friend]:https://github.com/cemerick/friend
[passport]:https://github.com/jaredhanson/passport
[rfc6749]:http://tools.ietf.org/html/rfc6749
[rfc6749-credentials]:http://tools.ietf.org/html/rfc6749#section-4.2.2
[pal-authenticate-img]:misc/pal-authenticate.png
[pal]:https://github.com/manifest/pal
[pal-oauth2]:https://github.com/manifest/pal-oauth2.git
[pal-google-oauth2]:https://github.com/manifest/pal-google-oauth2.git
[pal-facebook-oauth2]:https://github.com/manifest/pal-facebook-oauth2.git
[pal-example]:https://github.com/manifest/pal-example

