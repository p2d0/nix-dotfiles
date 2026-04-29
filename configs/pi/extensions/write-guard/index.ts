import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

// Port of tools.py::_write. Preserves the exact Edit-recipe error string so
// the model recovers to Edit on its next turn. The whitepaper's benchmark
// result depends on Write refusing whole-file rewrites of existing files
// (fires on ~57% of Polyglot exercises).
export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "write",
    label: "Write",
    description:
      "Create a NEW file with the given content. Refuses if the file already exists — use edit to modify existing files. Parent directories are created automatically.",
    parameters: Type.Object({
      file_path: Type.String({ description: "Absolute file path" }),
      content: Type.String({ description: "Full file content" }),
    }),
    async execute(_id, { file_path, content }) {
      if (existsSync(file_path)) {
        const recipe =
          `Error: Write refused — ${file_path} already exists.\n` +
          `\n` +
          `Write is only for creating NEW files. To change an existing file, use Edit:\n` +
          `  {"name": "Edit", "input": {"file_path": "${file_path}", ` +
          `"old_string": "<exact text currently in the file>", ` +
          `"new_string": "<replacement text>"}}\n` +
          `\n` +
          `If you do not already know the file's current content, Read it first to ` +
          `get the exact text for old_string. Include enough surrounding context ` +
          `(2-3 lines) to make old_string unique in the file.\n` +
          `\n` +
          `For multiple changes, emit multiple Edit calls — one per location. Do NOT ` +
          `retry Write; it will be refused again.`;
        return {
          content: [{ type: "text", text: recipe }],
          details: {},
          isError: true,
        };
      }

      try {
        mkdirSync(dirname(file_path), { recursive: true });
        writeFileSync(file_path, content, { encoding: "utf-8" });
        const lc = content.split("\n").length - (content.endsWith("\n") ? 1 : 0) +
          (content.length > 0 && !content.endsWith("\n") ? 1 : 0);
        return {
          content: [{ type: "text", text: `Created ${file_path} (${lc} lines)` }],
          details: {},
        };
      } catch (e) {
        return {
          content: [{ type: "text", text: `Error: ${(e as Error).message}` }],
          details: {},
          isError: true,
        };
      }
    },
  });
}
