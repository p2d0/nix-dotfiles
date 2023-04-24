# -*- mode: snippet -*-
# --
using System.Collections.Generic;
using FinFactory.Abstractions.Application.MediatR.InternalRequests;
using FinFactory.Documents.Dto;

namespace `(+yas-csharp/namespace)`
{
public class `(+yas/remove-from-filename "InternalRequestResponse")`InternalRequestResponse : InternalRequestResponse<$1>
{
public `(+yas/remove-from-filename "InternalRequestResponse")`InternalRequestResponse($1 result) : base(result) {}
$0
}
}