import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { normalizeWritePath } from "../write-guard/index.ts";
import { harnessIntervention } from "../_shared/intervention.ts";

// Read-before-edit guard.
//
// Small models routinely fire `edit` with an `oldText` they never actually saw
// — guessing at the current file contents — which either fails the exact-match
// requirement (wasting a turn) or, worse, matches the wrong span. Editors the
// user is used to (Claude Code et al.) enforce a simple invariant: a file must
// be Read before it can be Edited. We reproduce that here.
//
// Mechanism mirrors write-guard: we don't own pi's built-in `read`/`edit`
// tools, so we enforce at the event layer. We remember every file that was
// successfully `read` this session (`tool_result`, !isError), and block any
// `edit` whose target hasn't been read, redirecting the model to Read first.
//
// Why a separate extension from `read-guard`: read-guard trims an oversized
// read so it can't overflow a small context window — a different concern from
// the read-before-edit invariant. Keeping them apart keeps each single-purpose.
//
// A successful `edit` or `write` also marks the path as known: an edit only
// succeeds when the file was already read (we'd have blocked it otherwise), and
// a write means the model authored the file's contents, so a follow-up edit to
// either is legitimate without a re-read.

// Files read (or authored) in the current session. Module-scoped: one pi
// process drives one session at a time, and we clear on session_start.
export const readFiles = new Set<string>();

// pi's built-in tools use `path`; some prompts/older builds use `file_path`.
// Accept both so the guard is independent of which key the model emits.
export function resolveToolPath(
  input: Record<string, unknown>,
  cwd: string,
): string | undefined {
  const raw =
    typeof input.path === "string"
      ? input.path
      : typeof input.file_path === "string"
        ? input.file_path
        : undefined;
  if (!raw) return undefined;
  return normalizeWritePath(raw, cwd).path;
}

export function editBeforeReadReason(resolved: string): string {
  return (
    `File must be read first before edit — ${resolved} has not been read in ` +
    `this session.\n` +
    `\n` +
    `Read ${resolved} first to get the exact current text for oldText ` +
    `(whitespace and indentation must match exactly), then issue the Edit. ` +
    `Reading also lets you include enough surrounding context (2-3 lines) to ` +
    `make oldText unique in the file. Do NOT guess the file's contents.`
  );
}

export default function (pi: ExtensionAPI) {
  // New session (startup, /clear, /resume, reload) is a clean slate — what was
  // read in a previous session says nothing about the current one.
  pi.on("session_start", async () => {
    readFiles.clear();
  });

  // Record successful reads (and authored files) as "known".
  pi.on("tool_result", async (event, ctx) => {
    const name = String((event as any).toolName ?? "").toLowerCase();
    if (name !== "read" && name !== "edit" && name !== "write") return;
    if ((event as any).isError) return;
    const p = resolveToolPath(((event as any).input ?? {}) as Record<string, unknown>, ctx.cwd);
    if (p) readFiles.add(p);
  });

  // Block edits to files that were never read.
  pi.on("tool_call", async (event, ctx) => {
    if (String((event as any).toolName ?? "").toLowerCase() !== "edit") return;
    const input = ((event as any).input ?? {}) as Record<string, unknown>;
    const p = resolveToolPath(input, ctx.cwd);
    if (!p) return; // no resolvable path — let the edit tool surface its own error
    if (readFiles.has(p)) return; // already read this session — allow

    harnessIntervention(
      ctx,
      "the model tried to edit a file it hadn't read — redirected it to Read first.",
    );
    return { block: true, reason: editBeforeReadReason(p) };
  });
}
