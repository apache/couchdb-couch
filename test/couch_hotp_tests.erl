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

-module(couch_hotp_tests).

-include_lib("eunit/include/eunit.hrl").

test_vector_test() ->
    Key = <<"12345678901234567890">>,
    ?assertEqual(755224, couch_hotp:generate(Key, 0)),
    ?assertEqual(287082, couch_hotp:generate(Key, 1)),
    ?assertEqual(359152, couch_hotp:generate(Key, 2)),
    ?assertEqual(969429, couch_hotp:generate(Key, 3)),
    ?assertEqual(338314, couch_hotp:generate(Key, 4)),
    ?assertEqual(254676, couch_hotp:generate(Key, 5)),
    ?assertEqual(287922, couch_hotp:generate(Key, 6)),
    ?assertEqual(162583, couch_hotp:generate(Key, 7)),
    ?assertEqual(399871, couch_hotp:generate(Key, 8)),
    ?assertEqual(520489, couch_hotp:generate(Key, 9)).
