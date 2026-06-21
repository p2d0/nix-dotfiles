# C# Crash Diagnose & Fix

## Purpose

Systematic workflow for diagnosing and fixing C#/.NET crashes (NullReferenceException, InvalidOperationException, etc.) with root cause analysis and test coverage.

## When to Use

- Any unhandled exception or crash in C# code
- NullReferenceException, InvalidOperationException, ArgumentException, etc.
- Crashes in game loops, UI rendering, or async operations
- When a stack trace points to your codebase

## Workflow

### Phase 1: Reproduce & Isolate

1. **Reproduce the crash** using the minimal steps
2. **Capture the full stack trace** with exception details
3. **Identify the crash location** (file, line, method)
4. **Determine the trigger** (user action, game state, timing)

```
Crash Report Template:
- Exception: [type and message]
- Location: [file:line]
- Trigger: [what caused it]
- State: [game/UI state when crash occurred]
- Frequency: [always/sometimes/rare]
```

### Phase 2: Root Cause Analysis

1. **Trace the null/invalid value** backwards from crash point
2. **Check initialization paths** - is the object properly created?
3. **Check lifecycle** - is the object disposed/invalidated?
4. **Check async boundaries** - is state changing between awaits?
5. **Check dependencies** - are required services/objects available?

```csharp
// Common patterns to check:
// 1. Uninitialized fields
private SomeType field; // null until set

// 2. Async race conditions
await Task.Run(() => field = null); // another thread clears it

// 3. Disposed objects
someObject.Dispose(); // then access field

// 4. Missing null checks
return field.Property; // field could be null
```

### Phase 3: Implement Fix

1. **Choose the fix strategy**:
   - Add null check with fallback
   - Ensure proper initialization
   - Fix lifecycle management
   - Add defensive copying
   - Improve error handling

2. **Implement the fix** with minimal changes
3. **Add comments** explaining the fix if non-obvious
4. **Check for similar issues** in related code

```csharp
// Fix pattern examples:

// 1. Null check with fallback
public Statistics GetStatistics(RulesetInfo ruleset)
{
    if (ruleset?.OnlineID == null)
        return Statistics.Empty;

    return cache.GetOrCreate(ruleset.OnlineID, () => LoadStatistics(ruleset));
}

// 2. Defensive initialization
protected override void LoadAsync()
{
    statistics = Statistics.Empty; // safe default
    base.LoadAsync();
}

// 3. Lifecycle-aware access
public Statistics GetStatistics(RulesetInfo ruleset)
{
    if (!IsLoaded)
        throw new InvalidOperationException("Not loaded yet");

    return internalStatistics;
}
```

### Phase 4: Verify & Test

1. **Reproduce the original crash** - confirm it's fixed
2. **Test edge cases** - null inputs, empty states, race conditions
3. **Add unit test** if possible
4. **Add integration test** if needed
5. **Check related code** for similar patterns

```csharp
// Test pattern for null handling
[Test]
public void TestNullRulesetHandling()
{
    var manager = CreateManager();
    var result = manager.GetStatistics(null);
    Assert.That(result, Is.Not.Null);
    Assert.That(result.TotalScore, Is.EqualTo(0));
}

[Test]
public void TestConcurrentAccess()
{
    var manager = CreateManager();
    var tasks = Enumerable.Range(0, 10)
        .Select(_ => Task.Run(() => manager.GetStatistics(ruleset)))
        .ToList();

    Task.WaitAll(tasks.ToArray());
    Assert.Pass();
}
```

## Common C# Crash Patterns

### NullReferenceException
- Accessing property/method on null object
- Using result of method that returns null
- Uninitialized fields
- Dependency injection failures

### InvalidOperationException
- Accessing disposed objects
- Wrong state for operation
- Collection modified during enumeration
- Async state machine issues

### ArgumentException
- Invalid parameter values
- Out of range indices
- Invalid enum values
- Constraint violations

### Game Loop Specific
- Accessing objects after scene exit
- Modifying collections during iteration
- Race conditions in async operations
- UI updates on wrong thread

## Debugging Techniques

### Stack Trace Analysis
```
at Namespace.Class.Method(File.cs:line)
    ↑        ↑      ↑     ↑
   type    class  method file:line
```

### Null Tracing
1. Start at crash line
2. Identify which object is null
3. Trace back to where it should be set
4. Find why it wasn't set

### State Verification
```csharp
// Add temporary logging
Console.WriteLine($"Object state: {nameof(field)} = {field?.ToString() ?? "null"}");
Console.WriteLine($"IsLoaded: {IsLoaded}, IsDisposed: {IsDisposed}");
```

## Best Practices

1. **Fail fast** - check preconditions early
2. **Defensive programming** - validate inputs
3. **Clear error messages** - include context
4. **Graceful degradation** - fallback to safe state
5. **Test edge cases** - null, empty, invalid states
6. **Document assumptions** - when null is expected vs. unexpected

## Integration with Project

### osu! Framework Specifics
- Use `LoadAsync()` for initialization
- Respect `OnDispose()` lifecycle
- Handle `IUpdatable` carefully
- Use `Scheduled()` for thread-safe updates

### Testing Strategy
- Unit tests for business logic
- Integration tests for component interaction
- Visual tests for UI components
- Stress tests for concurrent access

## Output Format

When diagnosing a crash, provide:

1. **Root cause**: What specifically caused the crash
2. **Fix**: The code change with explanation
3. **Test**: How to verify the fix
4. **Prevention**: How to avoid similar issues

Example:
```
## Root Cause
`cacheKey()` called on null `RulesetInfo.OnlineID` when ruleset not fully loaded.

## Fix
Add null check in `GetStatisticsFor()` before calling `cacheKey()`.

## Test
Add test case with unloaded ruleset to verify fallback behavior.

## Prevention
Always validate ruleset state before accessing properties.
```
