import { describe, it, expect } from "vitest";
import { deriveSessionName } from "./index.ts";

describe("deriveSessionName", () => {
  it("uses at most the first 4 words, with an ellipsis when there are more", () => {
    expect(deriveSessionName("add a dark mode toggle to settings")).toBe("add a dark mode…");
  });

  it("keeps prompts of 4 words or fewer whole (no ellipsis)", () => {
    expect(deriveSessionName("add dark mode")).toBe("add dark mode");
    expect(deriveSessionName("one two three four")).toBe("one two three four");
  });

  it("never slices a word mid-way", () => {
    const name = deriveSessionName(
      "implement comprehensive authentication authorization subsystem now please",
    )!;
    // every space-separated token is a complete word from the input
    for (const w of name.replace(/…$/, "").split(" ")) {
      expect("implement comprehensive authentication authorization subsystem now please").toContain(w);
    }
    expect(name.endsWith("…")).toBe(true);
  });

  it("takes only the first line", () => {
    expect(deriveSessionName("fix the bug\nmore details here")).toBe("fix the bug");
  });

  it("collapses surrounding whitespace", () => {
    expect(deriveSessionName("   refactor   the   parser   ")).toBe("refactor the parser");
  });

  it("ignores slash-commands and bash lines", () => {
    expect(deriveSessionName("/resume")).toBeUndefined();
    expect(deriveSessionName("!ls -la")).toBeUndefined();
  });

  it("returns undefined for empty input", () => {
    expect(deriveSessionName("   ")).toBeUndefined();
    expect(deriveSessionName("")).toBeUndefined();
  });
});
