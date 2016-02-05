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

-module(couch_epochs).


-export([
    new/0,
    update/2
]).


new() ->
    [{node(), 0}].


update(Epochs, UpdateSeq) ->
    NewEpochs = case Epochs of
        [{Node, _} | _] when Node == node() ->
            % Current node is the current owner of this db
            Epochs;
        Epochs1 ->
            % This node is taking over ownership of this db
            % and marking the update sequence where it happened.
            [{node(), UpdateSeq} | Epochs1]
    end,
    % Its possible for a node to open a db and claim
    % ownership but never make a write to the db. This
    % removes nodes that claimed ownership but never
    % changed the database.
    remove_dup_epochs(NewEpochs).


% This is slightly relying on the udpate_seq's being sorted
% in epochs due to how we only ever push things onto the
% front. Although if we ever had a case where the update_seq
% is not monotonically increasing I don't know that we'd
% want to remove dupes (by calling a sort on the input to this
% function). So for now we don't sort but are relying on the
% idea that epochs is always sorted.
remove_dup_epochs([_]=Epochs) ->
    Epochs;
remove_dup_epochs([{N1, S}, {_N2, S}]) ->
    % Seqs match, keep the most recent owner
    [{N1, S}];
remove_dup_epochs([_, _]=Epochs) ->
    % Seqs don't match.
    Epochs;
remove_dup_epochs([{N1, S}, {_N2, S} | Rest]) ->
    % Seqs match, keep the most recent owner
    remove_dup_epochs([{N1, S} | Rest]);
remove_dup_epochs([{N1, S1}, {N2, S2} | Rest]) ->
    % Seqs don't match, recurse to check others
    [{N1, S1} | remove_dup_epochs([{N2, S2} | Rest])].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


upgrade_epochs_test() ->
    % By default the local node owns epochs
    % from update_seq 0.
    ?assertEqual([{node(), 0}], new()),

    % Fake epoch takeover, this should add the
    % local node and current update seq to the
    % epochs.
    OldEpochs = [
        {'someothernode@someotherhost', 0}
    ],

    TakeOverExpect = [
        {node(), 20},
        {'someothernode@someotherhost', 0}
    ],

    NewEpochs = update(OldEpochs, 20),
    ?assertEqual(TakeOverExpect, NewEpochs),

    % If we update again we don't add a new
    % epoch record or change the update seq.
    ?assertEqual(TakeOverExpect, update(NewEpochs, 30)).


-endif.
