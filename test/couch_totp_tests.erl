% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_totp_tests).

-include_lib("eunit/include/eunit.hrl").

test_vector_test() ->
    Key = <<"12345678901234567890">>,
    ?assertEqual(94287082, couch_totp:generate(Key, 59, 30)),
    ?assertEqual(07081804, couch_totp:generate(Key, 1111111109, 30)),
    ?assertEqual(14050471, couch_totp:generate(Key, 1111111111, 30)),
    ?assertEqual(89005924, couch_totp:generate(Key, 1234567890, 30)),
    ?assertEqual(69279037, couch_totp:generate(Key, 2000000000, 30)),
    ?assertEqual(65353130, couch_totp:generate(Key, 20000000000, 30)).
