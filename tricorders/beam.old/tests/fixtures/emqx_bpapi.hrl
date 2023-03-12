%%--------------------------------------------------------------------
%% Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-ifndef(EMQX_BPAPI_HRL).
-define(EMQX_BPAPI_HRL, true).

-define(TAB, bpapi).

-define(multicall, multicall).

-record(?TAB, {
    key :: {node() | ?multicall, emqx_bpapi:api()},
    version :: emqx_bpapi:api_version()
}).

-endif.
