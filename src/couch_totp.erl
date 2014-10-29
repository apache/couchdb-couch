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

-module(couch_totp).

-export([generate/3, generate/4]).

generate(Key, CounterSecs, StepSecs) ->
    generate(Key, CounterSecs, StepSecs, 8).

generate(Key, CounterSecs, StepSecs, OutputLen)
  when is_binary(Key), is_integer(CounterSecs), is_integer(StepSecs),
       is_integer(OutputLen) ->
    couch_hotp:generate(Key, CounterSecs div StepSecs, OutputLen).
