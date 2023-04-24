# -*- mode: snippet -*-
# name: FinFactory SET action
# --
import { `(+yas/to_upper_underscore_case (+yas/filename))`_ACTIONS , ${1:$$(+yas/to_upper_underscore_case (+yas/filename))}} from "../reducers/`(+yas/filename)`";

export const `(+yas/filename)` = (data: $1) => ({
type: $1_ACTIONS.SET,
data
});

export const re`(+yas/filename)` = () => ({
type: $1_ACTIONS.RESET
});
