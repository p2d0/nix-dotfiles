# -*- mode: snippet -*-
# --
using System;
using System.Threading.Tasks;
using FinFactory.Abstractions.Application.Authorization;
using MediatR;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using FinFactory.Core.Infrastructure.Controller;

namespace `(+yas-csharp/namespace)`
{
[Produces("application/json")]
[Route("api/`(s-replace "Controller" "" (s-lower-camel-case (+yas/filename)))`")]
public class `(+yas/filename)` : BaseController
{
private readonly IJwtTokenService _jwtTokenService;
private readonly IMediator _mediator;

public `(+yas/filename)`(IJwtTokenService jwtTokenService, IMediator mediator)
{
_jwtTokenService = jwtTokenService ?? throw new ArgumentNullException(nameof(jwtTokenService));
_mediator = mediator ?? throw new ArgumentNullException(nameof(mediator));
}
}

}
