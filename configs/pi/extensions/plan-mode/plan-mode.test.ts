import { describe, it, expect } from "vitest";
import { extractJsonArray, digestReports } from "./index.ts";
import type { SubCoderResult } from "../subagent/spawn.ts";

describe("extractJsonArray", () => {
  it("parses a bare JSON array", () => {
    expect(extractJsonArray('[{"label":"a","task":"t"}]')).toEqual([{ label: "a", task: "t" }]);
  });
  it("pulls the array out of surrounding prose / fences", () => {
    const text = 'Here is the plan:\n```json\n[{"q":"why?","options":["a","b"]}]\n```\nThanks!';
    expect(extractJsonArray(text)).toEqual([{ q: "why?", options: ["a", "b"] }]);
  });
  it("returns [] when there is no array", () => {
    expect(extractJsonArray("no json here")).toEqual([]);
    expect(extractJsonArray("")).toEqual([]);
  });
  it("returns [] on malformed JSON rather than throwing", () => {
    expect(extractJsonArray("[ this is not, valid json ]")).toEqual([]);
  });
});

describe("digestReports", () => {
  const mk = (over: Partial<SubCoderResult>): SubCoderResult => ({
    id: "1",
    label: "x",
    task: "t",
    exitCode: 0,
    report: "",
    messages: [],
    stderr: "",
    usage: { input: 0, output: 0, cost: 0, turns: 0, contextTokens: 0 },
    ...over,
  });

  it("renders each report under its label heading", () => {
    const out = digestReports([
      mk({ label: "auth", report: "uses JWT" }),
      mk({ label: "db", report: "postgres" }),
    ]);
    expect(out).toContain("### auth\nuses JWT");
    expect(out).toContain("### db\npostgres");
  });

  it("marks failed sub-coders instead of dropping them", () => {
    const out = digestReports([mk({ label: "web", exitCode: 1, errorMessage: "timeout" })]);
    expect(out).toContain("### web");
    expect(out).toContain("failed: timeout");
  });
});
