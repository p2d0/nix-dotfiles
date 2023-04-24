# -*- mode: snippet -*-
# --
using FinFactory.Api.Test.Fixtures;
using Xunit;

namespace `(+yas-csharp/namespace)`
{
[Collection("Fixtures")]
public class `(+yas/filename)`
{

private Fixture _fixture;

public `(+yas/filename)`(Fixture fixture)
{
_fixture = fixture;
}

[Fact]
public async void Test(){
$0
}

}
}
