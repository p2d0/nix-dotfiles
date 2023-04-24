# -*- mode: snippet -*-
# --
using System;
using System.Linq;
using System.Threading.Tasks;
using FinFactory.StockApplications.Db.Context;
using FinFactory.StockApplications.Domain.Applications.CreditParams.ContractFinancing;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;

namespace `(+yas-csharp/namespace)`
{
public interface I`(+yas/filename)`
{
}

public class `(+yas/filename)` : I`(+yas/filename)`
{
private readonly StockApplicationsContext _context;
public `(+yas/filename)`(StockApplicationsContext context)
{
_context = context;
}
}
}