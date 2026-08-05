/**
 * Review Queue Extension
 *
 * HTTP server for receiving code review comments from Emacs Doom.
 * Emacs sends POST /review with {comments: [{file, start, end, text}]}
 * Extension formats as markdown and sends as user message.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { createServer, type IncomingMessage, type ServerResponse } from "node:http";
import { resolve, relative } from "node:path";

const PORT = 1984;
const HOST = "0.0.0.0";

export default function (pi: ExtensionAPI) {
	let server: ReturnType<typeof createServer> | null = null;
	let pidFile: string | null = null;

	pi.on("session_start", async (_event, ctx) => {
		try {
			server = createServer((req, res) => handleRequest(req, res, ctx, pi));
			await new Promise<void>((resolve, reject) => {
				server?.listen(PORT, HOST, () => resolve());
				server?.on("error", reject);
			});

			// Write PID file so Emacs can verify correct pi instance
			const fs = await import("node:fs");
			const os = await import("node:os");
			const pidDir = resolve(os.homedir(), ".pi");
			fs.mkdirSync(pidDir, { recursive: true });
			pidFile = resolve(pidDir, "review-queue.pid");
			fs.writeFileSync(pidFile, `${process.pid}`);

			ctx.ui.notify(`Review queue server started on http://${HOST}:${PORT}`, "info");
			ctx.ui.setStatus("review-queue", `Listening :${PORT}`);
		} catch (err) {
			const msg = err instanceof Error ? err.message : String(err);
			ctx.ui.notify(`Review queue server failed: ${msg}`, "error");
		}
	});

	pi.on("session_shutdown", async (_event, _ctx) => {
		if (server) {
			server.close();
			server = null;
		}
		if (pidFile) {
			try {
				const fs = await import("node:fs");
				fs.unlinkSync(pidFile);
			} catch {
				// ignore
			}
			pidFile = null;
		}
	});
}

function handleRequest(req: IncomingMessage, res: ServerResponse, ctx: Parameters<Parameters<typeof pi.on>[1]>[1], pi: ExtensionAPI) {
	if (req.method === "POST" && req.url === "/review") {
		handleReview(req, res, ctx, pi);
	} else if (req.method === "GET" && req.url === "/health") {
		res.writeHead(200, { "Content-Type": "application/json" });
		res.end(JSON.stringify({ ok: true, pid: process.pid }));
	} else {
		res.writeHead(404, { "Content-Type": "application/json" });
		res.end(JSON.stringify({ error: "Not found" }));
	}
}

async function handleReview(req: IncomingMessage, res: ServerResponse, ctx: Parameters<Parameters<typeof pi.on>[1]>[1], pi: ExtensionAPI) {
	try {
		const body = await readBody(req);
		const data = JSON.parse(body) as { comments: Array<{ file: string; start: number; end: number; text: string }> };

		if (!data.comments || !Array.isArray(data.comments) || data.comments.length === 0) {
			res.writeHead(400, { "Content-Type": "application/json" });
			res.end(JSON.stringify({ error: "No comments provided" }));
			return;
		}

		// Format as markdown
		const repoRoot = ctx.cwd;
		const lines = [`Review comments (${data.comments.length}):`];
		lines.push("");
		lines.push(`Repository: ${repoRoot}`);
		lines.push("");

		for (let i = 0; i < data.comments.length; i++) {
			const c = data.comments[i];
			const fileRef = c.file.startsWith("/") ? c.file : resolve(repoRoot, c.file);
			const range = c.start === c.end ? String(c.start) : `${c.start}-${c.end}`;
			lines.push(`${i + 1}. ${c.file}:${range} — ${c.text}`);
		}

		const message = lines.join("\n");

		// Queue as steering message so it delivers after current turn, lets reviews queue
		pi.sendUserMessage(message, { deliverAs: "steer" });

		res.writeHead(200, { "Content-Type": "application/json" });
		res.end(JSON.stringify({ ok: true, count: data.comments.length }));
	} catch (err) {
		const msg = err instanceof Error ? err.message : String(err);
		res.writeHead(400, { "Content-Type": "application/json" });
		res.end(JSON.stringify({ error: msg }));
	}
}

function readBody(req: IncomingMessage): Promise<string> {
	return new Promise((resolve, reject) => {
		const chunks: Buffer[] = [];
		req.on("data", (chunk) => chunks.push(chunk));
		req.on("end", () => resolve(Buffer.concat(chunks).toString("utf8")));
		req.on("error", reject);
	});
}
