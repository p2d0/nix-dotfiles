import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import {
  runSubCoder,
  runSubCodersConcurrent,
  truncateReport,
  type SubCoderItem,
  type SubCoderResult,
} from "../subagent/spawn.ts";
import { SubCoderTracker } from "../subagent/tracker.ts";
import { currentModelId } from "../subagent/index.ts";
import { PlanStatus } from "./status.ts";
import { terminalColumns, truncateLineToWidth } from "../_shared/width.ts";

// Plan Mode — a Claude-Code-style "research, ask, then plan" flow.
//
// alt+p toggles plan mode (an indicator appears below the input). While it is
// on, submitting a prompt does NOT run a normal coding turn; instead the
// extension orchestrates:
//   1. decompose the request into 1-4 exploration tasks (a reasoning sub-coder),
//   2. dispatch those as read-only explorer sub-coders (isolated context; only
//      their concise reports survive — their transcripts never enter this window),
//   3. generate 1-3 clarifying questions with suggested answers (a sub-coder),
//   4. ask them via the UI (with a free-text "Other" option),
//   5. synthesize the reports + answers into a written plan in the main window,
//   6. exit plan mode.
//
// An extension can't call inference directly, so every reasoning step is a
// child little-coder (spawned via ../subagent/spawn.ts), and the final plan is
// injected as a normal turn via pi.sendUserMessage so it lands in the chat.
//
// alt+p is unbound by pi, so the extension can claim it cleanly without
// shadowing any built-in (shift+tab stays pi's thinking-level cycle — issue #47).

const honey = (s: string) => `\x1b[38;2;225;90;31m${s}\x1b[39m`;
const gray = (s: string) => `\x1b[90m${s}\x1b[39m`;
const INDICATOR_KEY = "plan-mode";

let planModeOn = false;
let orchestrating = false;
// True only while the synthesis turn runs — blocks edits/writes so plan mode
// produces a plan, not changes.
let planGuardActive = false;
let currentAbort: AbortController | null = null;
// Set just before the synthesis turn; consumed by before_agent_start to inject
// the planning instructions + research into the system prompt (kept out of the
// visible chat). Null at all other times.
let pendingSynthesis: { digest: string; answers: string } | null = null;
// True while the plan-writing turn is in flight; on its agent_end we prompt the
// user to approve & implement.
let synthesisActive = false;

function indicatorLines(): string[] {
  // Cap to terminal width — pi-tui throws on overflow (issue #48). The
  // indicator is short, but truncate for defense in depth so even a narrow
  // terminal (≤ 30 cols) doesn't crash on widget render.
  const raw = `${honey("◆")} ${honey("PLAN MODE")}  ${gray("(alt+p to exit)")}`;
  return [truncateLineToWidth(raw, terminalColumns())];
}

function setIndicator(ctx: any, on: boolean): void {
  if (!ctx?.hasUI) return;
  ctx.ui.setWidget(INDICATOR_KEY, on ? indicatorLines() : undefined, { placement: "belowEditor" });
}

// Pull the first balanced JSON array out of a model reply (small models love to
// wrap JSON in prose / fences). Returns [] on failure so callers can fall back.
export function extractJsonArray(text: string): any[] {
  const start = text.indexOf("[");
  const end = text.lastIndexOf("]");
  if (start < 0 || end <= start) return [];
  try {
    const v = JSON.parse(text.slice(start, end + 1));
    return Array.isArray(v) ? v : [];
  } catch {
    return [];
  }
}

async function reason(task: string, cwd: string, model: string | undefined, signal: AbortSignal): Promise<string> {
  const r = await runSubCoder({ id: "r", label: "planner", task, cwd, model, signal });
  return r.report;
}

interface ExploreTask {
  label: string;
  task: string;
}

async function decomposeTargets(
  prompt: string,
  cwd: string,
  model: string | undefined,
  signal: AbortSignal,
): Promise<ExploreTask[]> {
  const text = await reason(
    `You are PLANNING, not executing — do not write or change anything. Given this user request, ` +
      `list the 1-4 most useful independent areas to investigate before an implementation plan can be written. ` +
      `Output ONLY a JSON array of objects {"label": "<3-4 word name>", "task": "<a specific research instruction ` +
      `for an agent that can read this repo and browse online>"}. No prose.\n\nUser request:\n${prompt}`,
    cwd,
    model,
    signal,
  );
  const parsed = extractJsonArray(text)
    .filter((t) => t && typeof t.task === "string")
    .slice(0, 4)
    .map((t, i) => ({ label: String(t.label || `area ${i + 1}`).slice(0, 24), task: String(t.task) }));
  if (parsed.length > 0) return parsed;
  // Fallback: a single broad exploration of the request itself.
  return [{ label: "explore", task: `Investigate this repository to inform: ${prompt}` }];
}

interface Question {
  q: string;
  options: string[];
}

async function generateQuestions(
  prompt: string,
  digest: string,
  cwd: string,
  model: string | undefined,
  signal: AbortSignal,
): Promise<Question[]> {
  const text = await reason(
    `Based on the user's request and the research findings below, propose 1-3 clarifying questions whose ` +
      `answers would change the implementation plan. For each, give 1-3 short suggested answers. Output ONLY a ` +
      `JSON array of {"q": "<question>", "options": ["<short answer>", ...]}. No prose.\n\n` +
      `User request:\n${prompt}\n\nResearch findings:\n${digest}`,
    cwd,
    model,
    signal,
  );
  return extractJsonArray(text)
    .filter((q) => q && typeof q.q === "string")
    .slice(0, 3)
    .map((q) => ({
      q: String(q.q),
      options: (Array.isArray(q.options) ? q.options : []).map((o: any) => String(o)).filter(Boolean).slice(0, 3),
    }));
}

export function digestReports(results: SubCoderResult[]): string {
  return results
    .map((r) => `### ${r.label}\n${r.exitCode === 0 ? truncateReport(r.report) : `(failed: ${r.errorMessage || "no output"})`}`)
    .join("\n\n");
}

const OTHER_SENTINEL = "✎ Other (type my own answer)";

async function askQuestions(ctx: any, questions: Question[]): Promise<string> {
  const answered: string[] = [];
  for (const q of questions) {
    const options = [...q.options, OTHER_SENTINEL].filter(Boolean);
    let choice: string | undefined;
    try {
      choice = await ctx.ui.select(q.q, options);
    } catch {
      choice = undefined;
    }
    if (choice === undefined) {
      answered.push(`Q: ${q.q}\nA: (skipped)`);
      continue;
    }
    if (choice === OTHER_SENTINEL) {
      let typed: string | undefined;
      try {
        typed = await ctx.ui.input(q.q, "Type your answer");
      } catch {
        typed = undefined;
      }
      answered.push(`Q: ${q.q}\nA: ${typed?.trim() || "(no answer)"}`);
    } else {
      answered.push(`Q: ${q.q}\nA: ${choice}`);
    }
  }
  return answered.join("\n\n");
}

async function orchestrate(pi: ExtensionAPI, ctx: any, prompt: string): Promise<void> {
  orchestrating = true;
  const abort = new AbortController();
  currentAbort = abort;
  // One continuous timer for the whole plan-mode process — every phase widget
  // counts from t0, so the user sees total elapsed throughout (not just the
  // per-sub-coder timers).
  const t0 = Date.now();
  const tracker = new SubCoderTracker(ctx, { key: "plan-explorers", totalSince: t0 });
  const status = new PlanStatus(ctx);
  const model = currentModelId(ctx);

  // ESC (or Ctrl+C) cancels the plan: there's no agent turn running during the
  // research/question phases, so pi's built-in interrupt has nothing to abort —
  // we intercept the raw key ourselves and trip the AbortController.
  let escUnsub: (() => void) | null =
    ctx.ui?.onTerminalInput?.((data: string) => {
      if (data === "\x1b" || data === "\x03") {
        abort.abort();
        return { consume: true };
      }
      return undefined;
    }) ?? null;
  const dropEsc = () => {
    try {
      escUnsub?.();
    } catch {
      /* ignore */
    }
    escUnsub = null;
  };

  // The "submit a request" hint is done — plan mode is now working. Swap it for
  // the animated status line.
  setIndicator(ctx, false);
  try {
    status.start("deciding what to explore…", t0);
    const targets = await decomposeTargets(prompt, ctx.cwd, model, abort.signal);
    if (abort.signal.aborted) return;

    const items: SubCoderItem[] = targets.map((t, i) => ({
      id: String(i + 1),
      label: t.label,
      task: t.task,
      cwd: ctx.cwd,
    }));
    // Hand the visual off to the tracker for the research phase — running both
    // animated aboveEditor widgets at once made the panel flicker.
    status.stop();
    tracker.begin(items.map((it) => ({ id: it.id, label: it.label })));
    const results = await runSubCodersConcurrent(items, {
      model,
      signal: abort.signal,
      onUpdate: (all) => tracker.update(all),
    });
    tracker.end();
    if (abort.signal.aborted) return;

    const digest = digestReports(results);
    status.start("preparing clarifying questions…", t0);
    const questions = await generateQuestions(prompt, digest, ctx.cwd, model, abort.signal);
    if (abort.signal.aborted) return;

    // Questions are ready: stop the animation and stop intercepting ESC so the
    // dialogs (and the synthesis turn after) handle their own keys.
    status.stop();
    dropEsc();
    const answers = questions.length > 0 ? await askQuestions(ctx, questions) : "(no clarifying questions)";
    if (abort.signal.aborted) return;

    // Hand the synthesis to the main agent so the plan appears in the chat. The
    // user-visible message is their ORIGINAL request; the planning instructions
    // + research digest + answers are injected into this turn's system prompt
    // (see the before_agent_start handler) so they never show in the chat.
    // Edits/writes are blocked during this turn — plan mode produces a plan.
    planGuardActive = true;
    synthesisActive = true;
    pendingSynthesis = { digest, answers };
    ctx.ui?.notify?.("plan mode: writing the plan…", "info");
    pi.sendUserMessage(prompt);
  } catch (e) {
    ctx.ui?.notify?.(`plan mode failed: ${(e as Error)?.message ?? e}`, "error");
  } finally {
    if (abort.signal.aborted) ctx.ui?.notify?.("plan mode cancelled", "info");
    dropEsc();
    status.stop();
    tracker.end();
    orchestrating = false;
    currentAbort = null;
    // One request per plan-mode activation — drop back to normal mode.
    planModeOn = false;
    setIndicator(ctx, false);
  }
}

export default function (pi: ExtensionAPI) {
  // alt+p toggles plan mode. pi leaves alt+p unbound, so this doesn't collide
  // with any built-in and shift+tab stays bound to pi's thinking-level cycle
  // (issue #47).
  pi.registerShortcut("alt+p", {
    description: "Toggle plan mode",
    handler: (ctx: any) => {
      if (orchestrating) return; // mid-plan: ignore toggles
      planModeOn = !planModeOn;
      setIndicator(ctx, planModeOn);
      ctx.ui?.notify?.(planModeOn ? "plan mode on" : "plan mode off", "info");
    },
  });

  // Intercept a submitted prompt while plan mode is on and run the orchestration
  // instead of a normal coding turn.
  pi.on("input", async (event, ctx) => {
    if (!planModeOn) return;
    if ((event as any).source !== "interactive") return;
    const text = String((event as any).text ?? "").trim();
    // Let commands and bash through untouched even in plan mode.
    if (!text || text.startsWith("/") || text.startsWith("!")) return;
    if (orchestrating) {
      (ctx as any).ui?.notify?.("a plan is already in progress…", "warning");
      return { action: "handled" as const };
    }
    // Fire-and-forget: returning {handled} suppresses the normal turn; the
    // orchestration (dialogs, sub-coders, final synthesis) runs after.
    void orchestrate(pi, ctx, text);
    return { action: "handled" as const };
  });

  // Inject the planning instructions + research into the synthesis turn's
  // system prompt, so the chat shows only the user's original request and the
  // model's plan — never the verbose internal instructions.
  pi.on("before_agent_start", async (event) => {
    if (!pendingSynthesis) return;
    const { digest, answers } = pendingSynthesis;
    pendingSynthesis = null;
    const block =
      `\n\n## Plan Mode\n` +
      `The user's message is a request to PLAN, not to implement. Write a concrete, ` +
      `well-structured implementation plan as your reply, using the research findings ` +
      `and the user's answers below. Output the plan as text only — do NOT edit or ` +
      `create files.\n\n` +
      `### Research findings\n${digest}\n\n` +
      `### User's answers to clarifying questions\n${answers}`;
    return { systemPrompt: ((event as any).systemPrompt ?? "") + block };
  });

  // While synthesizing the plan, block any attempt to edit/write files.
  pi.on("tool_call", async (event, ctx) => {
    if (!planGuardActive) return;
    const name = String((event as any).toolName ?? "").toLowerCase();
    if (name !== "edit" && name !== "write") return;
    (ctx as any).ui?.notify?.("harness intervention: plan mode — emit the plan as text, not file changes.", "info");
    return {
      block: true,
      reason: "Plan mode is active: produce the implementation plan as text in your reply. Do NOT edit or create files.",
    };
  });

  // When a turn ends: if it was the plan-synthesis turn, the plan is now on
  // screen — ask the user to approve before implementing. On approval we hand
  // an "implement it" message to the agent with the edit/write guard lifted.
  pi.on("agent_end", async (_event, ctx) => {
    planGuardActive = false;
    if (!synthesisActive) return;
    synthesisActive = false;
    let choice: string | undefined;
    try {
      choice = await (ctx as any).ui?.select?.("Plan ready — implement it?", [
        "Approve & implement",
        "Keep planning (don't implement)",
      ]);
    } catch {
      choice = undefined;
    }
    if (choice === "Approve & implement") {
      // deliverAs: pi is still settling the just-ended synthesis turn (this
      // agent_end handler is itself part of that processing), so an immediate
      // send is rejected as "already processing" — queue it as a follow-up.
      pi.sendUserMessage("Implement the plan you just described — make the actual file changes now.", {
        deliverAs: "followUp",
      });
    } else {
      (ctx as any).ui?.notify?.(
        "plan not implemented — refine your request, or alt+p to leave plan mode",
        "info",
      );
    }
  });

  // A new/resumed session resets all plan-mode state.
  pi.on("session_start", async (_event, ctx) => {
    planModeOn = false;
    orchestrating = false;
    planGuardActive = false;
    synthesisActive = false;
    pendingSynthesis = null;
    if (currentAbort) currentAbort.abort();
    currentAbort = null;
    setIndicator(ctx, false);
  });
}
