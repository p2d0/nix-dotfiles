import { describe, it, expect, beforeEach } from "vitest";
import setupReadGuardEdit, { readFiles, resolveToolPath } from "./index.ts";

describe("resolveToolPath", () => {
  const cwd = "/home/me/proj";
  it("resolves relative paths against cwd", () => {
    expect(resolveToolPath({ path: "src/a.ts" }, cwd)).toBe("/home/me/proj/src/a.ts");
  });
  it("honors the file_path key", () => {
    expect(resolveToolPath({ file_path: "b.ts" }, cwd)).toBe("/home/me/proj/b.ts");
  });
  it("returns undefined with no path", () => {
    expect(resolveToolPath({ content: "x" }, cwd)).toBeUndefined();
  });
});

// Wire up the extension against a fake pi, capturing each registered handler.
function setup() {
  const handlers: Record<string, (...args: any[]) => any> = {};
  const pi = {
    on(name: string, h: (...args: any[]) => any) {
      handlers[name] = h;
    },
  };
  setupReadGuardEdit(pi as any);
  return handlers;
}

function makeCtx(cwd: string) {
  const notifies: string[] = [];
  return { cwd, notifies, ui: { notify: (m: string) => notifies.push(m) } };
}

describe("read-before-edit guard", () => {
  const cwd = "/home/me/proj";
  beforeEach(() => readFiles.clear());

  it("blocks an edit to a file that was never read", async () => {
    const h = setup();
    const ctx = makeCtx(cwd);
    const result = await h.tool_call({ toolName: "edit", input: { path: "a.ts", edits: [] } }, ctx);
    expect(result?.block).toBe(true);
    expect(result.reason).toContain("must be read first");
    expect(result.reason).toContain("/home/me/proj/a.ts");
    expect(ctx.notifies[0]).toMatch(/harness intervention:.*Read first/i);
  });

  it("allows an edit after the file was successfully read", async () => {
    const h = setup();
    const ctx = makeCtx(cwd);
    await h.tool_result({ toolName: "read", isError: false, input: { path: "a.ts" } }, ctx);
    const result = await h.tool_call({ toolName: "edit", input: { path: "a.ts", edits: [] } }, ctx);
    expect(result).toBeUndefined();
    expect(ctx.notifies).toHaveLength(0);
  });

  it("does not count a failed read as having read the file", async () => {
    const h = setup();
    const ctx = makeCtx(cwd);
    await h.tool_result({ toolName: "read", isError: true, input: { path: "a.ts" } }, ctx);
    const result = await h.tool_call({ toolName: "edit", input: { path: "a.ts" } }, ctx);
    expect(result?.block).toBe(true);
  });

  it("treats a freshly written file as known (write then edit is allowed)", async () => {
    const h = setup();
    const ctx = makeCtx(cwd);
    await h.tool_result({ toolName: "write", isError: false, input: { path: "new.ts" } }, ctx);
    const result = await h.tool_call({ toolName: "edit", input: { path: "new.ts" } }, ctx);
    expect(result).toBeUndefined();
  });

  it("clears known files on session_start", async () => {
    const h = setup();
    const ctx = makeCtx(cwd);
    await h.tool_result({ toolName: "read", isError: false, input: { path: "a.ts" } }, ctx);
    await h.session_start();
    const result = await h.tool_call({ toolName: "edit", input: { path: "a.ts" } }, ctx);
    expect(result?.block).toBe(true);
  });

  it("matches reads and edits that use different path spellings for the same file", async () => {
    const h = setup();
    const ctx = makeCtx(cwd);
    // read with cwd-relative, edit with the absolute form of the same file
    await h.tool_result({ toolName: "read", isError: false, input: { path: "src/a.ts" } }, ctx);
    const result = await h.tool_call(
      { toolName: "edit", input: { path: "/home/me/proj/src/a.ts" } },
      ctx,
    );
    expect(result).toBeUndefined();
  });

  it("ignores non-edit tool calls", async () => {
    const h = setup();
    const ctx = makeCtx(cwd);
    const result = await h.tool_call({ toolName: "grep", input: { pattern: "x" } }, ctx);
    expect(result).toBeUndefined();
  });
});
