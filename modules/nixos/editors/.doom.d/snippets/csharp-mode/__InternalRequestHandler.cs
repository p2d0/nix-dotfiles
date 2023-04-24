# -*- mode: snippet -*-
# name: InternalRequestHandler
# --
using System.Threading;
using System.Threading.Tasks;
using FinFactory.Domain.InternalRequests.Documents;
using FinFactory.StockApplications.Core.Queries;
using MediatR;

namespace `(+yas-csharp/namespace)`
{
public class `(+yas/remove-from-filename "InternalRequestHandler")`InternalRequestHandler : IRequestHandler<`(+yas/remove-from-filename "InternalRequestHandler")`InternalRequest, `(+yas/remove-from-filename "InternalRequestHandler")`InternalRequestResponse>
{

public async Task<`(+yas/remove-from-filename "InternalRequestHandler")`InternalRequestResponse> Handle(
`(+yas/remove-from-filename "InternalRequestHandler")`InternalRequest request,
CancellationToken cancellationToken)
{
return new `(+yas/remove-from-filename "InternalRequestHandler")`InternalRequestResponse(null);
}
}
}