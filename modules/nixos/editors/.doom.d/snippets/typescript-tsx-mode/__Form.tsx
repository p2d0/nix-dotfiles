# -*- mode: snippet -*-
# --
import * as React from 'react';

import {
EachForm,
} from 'Application/components';

export class `(+yas/filename)` extends React.Component {
render() {
const routes = [
{
path: 'initialization',
component: ${1:`(+yas/filename)`Step1}
},

];

return <EachForm routes={routes} />;
}
}
