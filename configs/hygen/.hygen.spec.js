const { helpers } = require("./.hygen.js");
const fs = require("fs");

describe("hygen_module", function() {
  it("should find csproj", function() {
    fs.writeFileSync("../../test.csproj","");
    const namespace = helpers.namespace()
    expect(namespace).toBe("Hygen.Command");
    fs.rmSync("../../test.csproj");
  })
})
