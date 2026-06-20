import { describe, it, expect, beforeEach } from "vitest";
import { getSessionStore, resetSessionStore } from "./index.ts";

// Lightweight harness — exercise the store by accessing internal helpers.
// Full tool execution (registerTool) requires a pi runtime; deferred to
// Phase 12 smoke tests.

describe("evidence session store", () => {
  beforeEach(() => {
    resetSessionStore("test-session");
    resetSessionStore(); // default
  });

  it("starts empty", () => {
    expect(getSessionStore("test-session")).toEqual([]);
  });

  it("reset clears entries", () => {
    // Nothing to seed — just confirm reset is idempotent
    resetSessionStore("test-session");
    expect(getSessionStore("test-session")).toEqual([]);
  });

  it("isolates sessions by id", () => {
    // Different session IDs yield independent empty stores
    expect(getSessionStore("s1")).toEqual([]);
    expect(getSessionStore("s2")).toEqual([]);
    expect(getSessionStore("s1")).not.toBe(getSessionStore("s2"));
  });
});
