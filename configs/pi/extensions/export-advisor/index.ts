/**
 * export-advisor — Pi extension
 *
 * Same context assembly as rpiv-advisor (tool inventory + session messages),
 * but writes it to a file and copies to clipboard instead of calling advisor.
 * Uses wl-copy with stdin redirected directly to file descriptor.
 */

import {
  buildSessionContext,
  convertToLlm,
  type ExtensionAPI,
  type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import { spawn } from "node:child_process";
import { writeFileSync } from "node:fs";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { ADVISOR_SYSTEM_PROMPT } from "./prompt.js";

const TOOL_NAME = "export-advisor";
const TOOL_LABEL = "Export Advisor Context";

const EXPORT_DESCRIPTION =
  "Export the full advisor context (tool inventory + session messages) to a file and copy to clipboard. " +
  "Use when you need to share context externally or debug the advisor call. " +
  "Takes NO parameters — when you call export-advisor(), the entire conversation history and tool inventory " +
  "are written to ~/.config/pi-export-advisor/context.json and copied to clipboard.";

const DEFAULT_PROMPT_SNIPPET =
  "Export full advisor context to file and clipboard for external review or debugging";

const DEFAULT_PROMPT_GUIDELINES: string[] = [
  "Call `export-advisor` when you need to share context with an external reviewer or tool.",
  "Call `export-advisor` before escalating to user when they request context export.",
];

const CONTEXT_FILE = join(tmpdir(), "pi-export-advisor-context.txt");

function copyFileToClipboard(filePath: string): Promise<{ ok: boolean; error?: string }> {
  return new Promise((resolve) => {
    try {
      const uri = `file://${filePath}\n`;
      const proc = spawn("wl-copy", ["-t", "text/uri-list"]);
      proc.stdin.write(uri);
      proc.stdin.end();
      proc.on("error", (err) => resolve({ ok: false, error: err.message }));
      proc.on("exit", (code) => {
        if (code === 0) resolve({ ok: true });
        else resolve({ ok: false, error: `wl-copy exited with code ${code}` });
      });
    } catch (err: any) {
      resolve({ ok: false, error: err.message });
    }
  });
}

function buildInventoryMessage(pi: ExtensionAPI): string {
  const tools = pi.getAllTools();
  const lines = ["# Tool Inventory"];
  for (const tool of tools) {
    lines.push(`- ${tool.name}: ${tool.description}`);
  }
  return lines.join("\n");
}

function messagesToText(messages: Array<{ role: string; content: Array<{ type: string; text?: string }> }>): string {
  const lines: string[] = [];
  for (const msg of messages) {
    const role = msg.role;
    const text = msg.content
      .filter((c): c is { type: string; text: string } => c.type === "text" && typeof c.text === "string")
      .map((c) => c.text)
      .join("\n");
    if (text) {
      lines.push(`[${role}]`);
      lines.push(text);
      lines.push("");
    }
  }
  return lines.join("\n").trim();
}

async function doExport(pi: ExtensionAPI, ctx: ExtensionContext): Promise<{ ok: boolean; message: string }> {
  try {
    const { messages: sessionMessages } = buildSessionContext(
      ctx.sessionManager.getEntries(),
      ctx.sessionManager.getLeafId(),
    );
    const llmMessages = convertToLlm(sessionMessages);
    const inventoryText = buildInventoryMessage(pi);
    const sessionText = messagesToText(llmMessages);

    const fullContext = [
      "# Advisor Context Export",
      `# Generated: ${new Date().toISOString()}`,
      "",
      ADVISOR_SYSTEM_PROMPT,
      "",
      inventoryText,
      "",
      "# Session Messages",
      sessionText,
    ].join("\n");

    writeFileSync(CONTEXT_FILE, fullContext, "utf-8");

    const clipResult = await copyFileToClipboard(CONTEXT_FILE);

    if (!clipResult.ok) {
      return { ok: false, message: `Context exported to ${CONTEXT_FILE}, but clipboard copy failed: ${clipResult.error}` };
    }

    return { ok: true, message: `Advisor context exported to ${CONTEXT_FILE} and copied to clipboard.` };
  } catch (err: any) {
    return { ok: false, message: `Export failed: ${err.message}` };
  }
}

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: TOOL_NAME,
    label: TOOL_LABEL,
    description: EXPORT_DESCRIPTION,
    promptSnippet: DEFAULT_PROMPT_SNIPPET,
    promptGuidelines: DEFAULT_PROMPT_GUIDELINES,
    parameters: Type.Object({}),

    async execute(_toolCallId, _params, _signal, _onUpdate, ctx) {
      const result = await doExport(pi, ctx);
      return {
        content: [{ type: "text" as const, text: result.message }],
        details: {},
      };
    },
  });

  pi.registerCommand("export-advisor", {
    description: "Export advisor context to file and clipboard",
    handler: async (_args, ctx) => {
      const result = await doExport(pi, ctx);
      ctx.ui.notify(result.message, result.ok ? "info" : "error");
    },
  });
}
