# Example: Fixing NullReferenceException in Statistics Caching

## Problem

```
System.NullReferenceException: Object reference not set to an instance of an object.
at osu.Game.Rulesets.MOsu.LocalUserManager.cacheKey(RulesetInfo ruleset)
at osu.Game.Rulesets.MOsu.LocalUserManager.GetStatisticsFor(RulesetInfo ruleset)
```

## Root Cause Analysis

The `cacheKey` method accesses `ruleset.OnlineID` without null checking:

```csharp
// Before (buggy)
private string cacheKey(RulesetInfo ruleset)
{
    return $"statistics_{ruleset.OnlineID}"; // OnlineID can be null
}
```

When `GetStatisticsFor` is called with a ruleset that hasn't been fully initialized, `OnlineID` is null, causing the exception.

## Fix

```csharp
// After (fixed)
private string cacheKey(RulesetInfo ruleset)
{
    if (ruleset?.OnlineID == null)
        return null;

    return $"statistics_{ruleset.OnlineID}";
}

public Statistics GetStatisticsFor(RulesetInfo ruleset)
{
    var key = cacheKey(ruleset);
    if (key == null)
        return Statistics.Empty; // Safe fallback

    return cache.GetOrCreate(key, () => LoadStatistics(ruleset));
}
```

## Test Coverage

```csharp
[Test]
public void TestGetStatisticsFor_NullOnlineID()
{
    var manager = CreateLocalUserManager();
    var ruleset = new RulesetInfo { OnlineID = null };

    var statistics = manager.GetStatisticsFor(ruleset);

    Assert.That(statistics, Is.Not.Null);
    Assert.That(statistics.TotalScore, Is.EqualTo(0));
}

[Test]
public void TestGetStatisticsFor_ValidRuleset()
{
    var manager = CreateLocalUserManager();
    var ruleset = new RulesetInfo { OnlineID = 1 };

    var statistics = manager.GetStatisticsFor(ruleset);

    Assert.That(statistics, Is.Not.Null);
}
```

## Prevention

- Always validate input parameters
- Use nullable reference types where appropriate
- Add unit tests for edge cases
- Consider using `ArgumentNullException` for invalid inputs in public APIs
