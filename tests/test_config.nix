{ lib }:

lib.runTests {
  testEasy = {
    expr = {kek = "y";};
    expected = { kek = "y";};
  };
}
