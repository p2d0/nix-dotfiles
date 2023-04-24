# -*- mode: snippet -*-
# --
import Validator from 'instant-validation';
import { requiredRule } from 'instant-validation/build/rules';

import { ValidatorMessages } from 'constants/index';

export const createValidation = () =>
new Validator<$1>({
$2: [
{
rule: requiredRule,
message: ValidatorMessages.requiredField
}
]
});
