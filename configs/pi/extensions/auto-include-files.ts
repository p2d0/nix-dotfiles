/**
 * Auto-Include Files Extension
 *
 * Scans AGENTS.md / CLAUDE.md context files for @filepath references
 * and forces the model to read the referenced files by injecting their
 * contents into the system prompt.
 *
 * Syntax: @path/to/file (resolved relative to the AGENTS.md's directory
 * or the cwd if absolute).
 */
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { readFileSync } from "node:fs";
import { join, isAbsolute, dirname } from "node:path";

// Match @filepath but not @[filepath] (user mentions)
const REF_RE = /@([^\s\[\]]+)/g;

const cache = new Map<string, { content: string; mtime: number }>();

function readFileCached(filePath: string): string | null {
  try {
    const stat = require("node:fs").statSync(filePath);
    const cached = cache.get(filePath);
    if (cached && cached.mtime === stat.mtimeMs) return cached.content;
    const content = readFileSync(filePath, "utf-8");
    cache.set(filePath, { content, mtime: stat.mtimeMs });
    return content;
  } catch {
    return null;
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("before_agent_start", (event) => {
    const { systemPrompt, systemPromptOptions } = event;
    const contextFiles = systemPromptOptions?.contextFiles;
    if (!contextFiles || contextFiles.length === 0) return;

    const included = new Set<string>();
    const injections: string[] = [];

    for (const cf of contextFiles) {
      const refs = [...cf.content.matchAll(REF_RE)];
      for (const match of refs) {
        const rawPath = match[1];
        // Resolve relative to the context file's directory
        const resolvedPath = isAbsolute(rawPath)
          ? rawPath
          : join(dirname(cf.path), rawPath);

        if (included.has(resolvedPath)) continue;
        included.add(resolvedPath);

        const content = readFileCached(resolvedPath);
        if (!content) {
          injections.push(`<!-- @${rawPath} (not found: ${resolvedPath}) -->`);
          continue;
        }

        injections.push(
          `<!-- Auto-included from @${rawPath} -->\n` +
          `\`\`\`${resolvedPath}\n${content}\n\`\`\``,
        );
      }
    }

    if (injections.length === 0) return;

    const appendix = `\n\n## Auto-Included Files\n\n${injections.join("\n\n")}`;
    return { systemPrompt: systemPrompt + appendix };
  });
}
