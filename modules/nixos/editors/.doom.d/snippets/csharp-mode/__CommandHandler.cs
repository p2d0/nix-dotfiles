# -*- mode: snippet -*-
# --
using System;
using System.Collections.Generic;
using System.Threading;
using MediatR;
using System.Threading.Tasks;

namespace `(+yas-csharp/namespace)`
{
    public class `(+yas/filename)`
    : IRequestHandler<`(+yas-csharp/command-base)`Command, `(+yas-csharp/command-base)`CommandResult>
    {
        public async Task<`(+yas-csharp/command-base)`CommandResult> Handle(`(+yas-csharp/command-base)`Command command, CancellationToken cancellationToken)
        {
         $0
            return new `(+yas-csharp/command-base)`CommandResult(200, string.Empty);
        }
    }
}
