%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 
%% @author Nick Gerakines <nick@gerakines.net>
-module(githubby).
-export([
    user_info/2,
    user_repos/2,
    user_watched_repos/2,
    user_repos_commits/3,
    user_repos_commits/4
]).

-define(API_BASE, "http://github.com/api/v2/json").

user_info({Login, Token}, UserName) when is_list(Token), is_list(UserName) ->
    request_url(get, Login, Token, ?API_BASE ++ "/user/show/" ++ UserName).

user_repos({Login, Token}, UserName) when is_list(Token), is_list(UserName) ->
    request_url(get, Login, Token, ?API_BASE ++ "/repos/show/" ++ UserName).

user_watched_repos({Login, Token}, UserName) when is_list(Token), is_list(UserName) ->
    request_url(get, Login, Token, ?API_BASE ++ "/repos/watched/" ++ UserName).

user_repos_commits({Login, Token}, UserName, Repos) ->
    user_repos_commits({Login, Token}, UserName, Repos, "master").

user_repos_commits({Login, Token}, UserName, Repos, Branch) when is_list(Token), is_list(UserName) ->
    Url = lists:concat([?API_BASE, "/commits/list/", UserName, "/", Repos, "/", Branch]),
    request_url(get, Login, Token, Url).

%% @private
request_url(get, Login, Token, Url) ->
    case http:request(get, {Url, headers(Login, Token)}, [{timeout, 6000}], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            mochijson2:decode(Body);
        Other -> exit({unexpected_response, Other})
    end.

%% @private
headers(Login, Token) ->
    [{"User-Agent", "GitHubby/0.1"}, {"Host", "github.com"}, {"login", Login}, {"token", Token}].
