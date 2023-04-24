# -*- mode: snippet -*-
# --
using System.Runtime.Serialization;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;

namespace `(+yas-csharp/namespace)` {
[JsonConverter(typeof(StringEnumConverter))]
public enum `(+yas/filename)`
{

}
}
