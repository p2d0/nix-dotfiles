import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { existsSync } from "node:fs";
import { isAbsolute, join } from "node:path";
import { harnessIntervention } from "../_shared/intervention.ts";

/**
 * Resolve a write `path` argument to a concrete on-disk path.
 *
 * Two deterministic rewrites:
 *
 * 1. `"/<single-segment>"` (e.g. `/foo.md`) → `<cwd>/<single-segment>`.
 *    Background: the model has been seen to anchor at filesystem root when
 *    given an "Absolute file path" schema and no obvious directory context.
 *    Genuine system-path writes always include at least one intermediate
 *    directory (`/etc/X`, `/tmp/Y/Z`), so a root + bare filename is almost
 *    always a mistake. Rewriting to cwd matches user intent and avoids
 *    accidentally writing to `/`.
 *
 * 2. Bare filename / relative path (no leading slash) → resolved against cwd.
 *
 * Anything else (absolute path with at least one intermediate directory) is
 * left untouched.
 */
export function normalizeWritePath(
  filePath: string,
  cwd: string = process.cwd(),
): { path: string; rewrittenFrom?: string } {
  if (/^\/[^/]+$/.test(filePath)) {
    return { path: join(cwd, filePath.slice(1)), rewrittenFrom: filePath };
  }
  if (!isAbsolute(filePath)) {
    return { path: join(cwd, filePath) };
  }
  return { path: filePath };
}

// Read whichever key carries the destination path. pi's built-in `write` uses
// `path`; older little-coder builds and some prompts use `file_path`. We accept
// both so the guard is independent of which write implementation is in play.
function pathKey(input: Record<string, unknown>): "path" | "file_path" | undefined {
  if (typeof input.path === "string") return "path";
  if (typeof input.file_path === "string") return "file_path";
  return undefined;
}

function editRecipe(resolved: string): string {
  return (
    `Write refused — ${resolved} already exists.\n` +
    `\n` +
    `Write is for creating NEW files only. To change an existing file, use Edit:\n` +
    `  {"name": "edit", "input": {"path": "${resolved}", ` +
    `"edits": [{"oldText": "<exact text currently in the file>", ` +
    `"newText": "<replacement text>"}]}}\n` +
    `\n` +
    `If you do not already know the file's current content, Read it first to get the ` +
    `exact text for oldText (whitespace and indentation must match). Include enough ` +
    `surrounding context (2-3 lines) to make oldText unique in the file.\n` +
    `\n` +
    `For multiple changes, pass multiple entries in edits[] — one per location. Do NOT ` +
    `retry Write; it will be refused again.`
  );
}

// Port of tools.py::_write's guard. The whitepaper's benchmark result depends
// on Write refusing whole-file rewrites of existing files (fires on ~57% of
// Polyglot exercises). The earlier implementation registered a *custom* `write`
// tool to enforce this — but pi ships its own built-in `write`
// (`core/tools/write.js`, "overwrites if it does") which shadowed the custom
// one, so on current pi the guard never fired and existing files were silently
// rewritten. We now enforce at the `tool_call` event instead, which fires for
// whichever `write` implementation runs and lets us both normalize the path in
// place and block the call before it executes.
export default function (pi: ExtensionAPI) {
  pi.on("tool_call", async (event, ctx) => {
    if (String((event as any).toolName ?? "").toLowerCase() !== "write") return;
    const input = ((event as any).input ?? {}) as Record<string, unknown>;
    const key = pathKey(input);
    if (!key) return;

    const { path: resolved } = normalizeWritePath(String(input[key]), ctx.cwd);
    // Normalize in place so the executing write (built-in or custom) lands on
    // the resolved path even when we don't block (e.g. the `/foo.md` → cwd fix).
    input[key] = resolved;

    if (!existsSync(resolved)) return; // new file — allow the write through

    harnessIntervention(
      ctx,
      "small models can't rewrite whole files — redirected the model to Edit.",
    );
    return { block: true, reason: editRecipe(resolved) };
  });
}
