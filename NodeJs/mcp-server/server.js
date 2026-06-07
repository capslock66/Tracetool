#!/usr/bin/env node
// TraceTool MCP Server
// Exposes TraceTool viewer tracing as MCP tools for Claude Code / Claude agents.
// The TraceTool viewer must be running and accessible at the configured host.

"use strict";

const { McpServer } = require("@modelcontextprotocol/sdk/server/mcp.js");
const { StdioServerTransport } = require("@modelcontextprotocol/sdk/server/stdio.js");
const { z } = require("zod");
const path = require("path");
const fs = require("fs");

const _log = fs.createWriteStream(path.join(__dirname, "debug.log"), { flags: "a" });
process.stderr.write = (msg) => { _log.write(msg); return true; };

process.stderr.write("Starting TraceTool MCP server...\n");

// redirect all console.log to process. stdout can't used !!!
console.log = (...args) => process.stderr.write(args.join(' ') + '\n');

// Load the tracetool library from the sibling source folder
const ttrace = require(path.join(__dirname, "../Source/lib/tracetool.js"));

// --- Configuration ---
// Default viewer host. Override via TRACETOOL_HOST env var or the set_host tool.
ttrace.host = process.env.TRACETOOL_HOST || "127.0.0.1:81";

// --- MCP Server ---
const server = new McpServer({
  name: "tracetool",
  version: "1.0.0",
});

// -----------------------------------------------------------------------
// Tool: set_host
// -----------------------------------------------------------------------
server.tool(
  "tracetool_set_host",
  "Set the TraceTool viewer host (e.g. '127.0.0.1:81'). Call this before other tools if the viewer runs on a different port.",
  { host: z.string().describe("host:port of the TraceTool viewer") },
  async ({ host }) => {
    ttrace.host = host;
    return { content: [{ type: "text", text: `TraceTool host set to ${host}` }] };
  }
);

// -----------------------------------------------------------------------
// Tool: clear_all
// -----------------------------------------------------------------------
server.tool(
  "tracetool_clear_all",
  "Clear all traces in the main TraceTool viewer window.",
  {},
  async () => {
    ttrace.clearAll();
    return { content: [{ type: "text", text: "TraceTool viewer cleared." }] };
  }
);

// -----------------------------------------------------------------------
// Tool: send
// -----------------------------------------------------------------------
server.tool(
  "tracetool_send",
  "Send a trace message to the TraceTool viewer.",
  {
    level:    z.enum(["debug", "warning", "error"]).default("debug").describe("Trace level"),
    message:  z.string().describe("Main (left column) message text"),
    right:    z.string().optional().describe("Optional right-column message"),
  },
  async ({ level, message, right }) => {
    const node = ttrace[level].send(message, right);
    void node; // node can be used for chaining, but we don't need it here
    return { content: [{ type: "text", text: `[${level}] ${message}${right ? " | " + right : ""}` }] };
  }
);

// -----------------------------------------------------------------------
// Tool: indent
// -----------------------------------------------------------------------
server.tool(
  "tracetool_indent",
  "Send a trace message and indent subsequent traces under it (like entering a function scope).",
  {
    level:   z.enum(["debug", "warning", "error"]).default("debug"),
    message: z.string().describe("Message to show at the indent level"),
    right:   z.string().optional(),
  },
  async ({ level, message, right }) => {
    ttrace[level].indent(message, right, undefined, true);
    return { content: [{ type: "text", text: `Indented: [${level}] ${message}` }] };
  }
);

// -----------------------------------------------------------------------
// Tool: unindent
// -----------------------------------------------------------------------
server.tool(
  "tracetool_unindent",
  "Close the current indent level (like exiting a function scope). Optionally send a closing message.",
  {
    level:   z.enum(["debug", "warning", "error"]).default("debug"),
    message: z.string().optional().describe("Optional closing message"),
    right:   z.string().optional(),
  },
  async ({ level, message, right }) => {
    ttrace[level].unIndent(message, right, undefined, true);
    return { content: [{ type: "text", text: `Unindented${message ? ": " + message : ""}` }] };
  }
);

// -----------------------------------------------------------------------
// Tool: send_value
// -----------------------------------------------------------------------
server.tool(
  "tracetool_send_value",
  "Send a JavaScript object/value to the TraceTool viewer, displayed as an expandable tree.",
  {
    level:    z.enum(["debug", "warning", "error"]).default("debug"),
    message:  z.string().describe("Label for the value"),
    value:    z.string().describe("JSON-encoded value to inspect (will be parsed)"),
    maxLevel: z.number().int().min(1).max(10).default(3).describe("Max tree depth"),
  },
  async ({ level, message, value, maxLevel }) => {
    let parsed;
    try {
      parsed = JSON.parse(value);
    } catch {
      parsed = value;
    }
    ttrace[level].sendValue(message, parsed, maxLevel);
    return { content: [{ type: "text", text: `Sent value '${message}' to viewer.` }] };
  }
);

// -----------------------------------------------------------------------
// Tool: send_xml
// -----------------------------------------------------------------------
server.tool(
  "tracetool_send_xml",
  "Send an XML string to the TraceTool viewer with syntax highlighting.",
  {
    level:   z.enum(["debug", "warning", "error"]).default("debug"),
    message: z.string().describe("Label for the XML trace"),
    xml:     z.string().describe("XML text to display"),
  },
  async ({ level, message, xml }) => {
    ttrace[level].sendXml(message, xml);
    return { content: [{ type: "text", text: `Sent XML '${message}' to viewer.` }] };
  }
);

// -----------------------------------------------------------------------
// Tool: send_table
// -----------------------------------------------------------------------
server.tool(
  "tracetool_send_table",
  "Send a table (array of objects) to the TraceTool viewer.",
  {
    level:   z.enum(["debug", "warning", "error"]).default("debug"),
    message: z.string().describe("Label for the table"),
    rows:    z.string().describe("JSON array of row objects, e.g. [{col1: 'a', col2: 1}, ...]"),
  },
  async ({ level, message, rows }) => {
    let parsed;
    try {
      parsed = JSON.parse(rows);
    } catch {
      return { content: [{ type: "text", text: "Error: rows must be valid JSON array." }] };
    }
    ttrace[level].sendTable(message, parsed);
    return { content: [{ type: "text", text: `Sent table '${message}' (${parsed.length} rows) to viewer.` }] };
  }
);

// -----------------------------------------------------------------------
// Tool: enter_method / exit_method
// -----------------------------------------------------------------------
server.tool(
  "tracetool_enter_method",
  "Send an 'Enter methodName' trace with indent — use at the start of a logical operation.",
  {
    level:   z.enum(["debug", "warning", "error"]).default("debug"),
    method:  z.string().describe("Method or operation name"),
    right:   z.string().optional(),
  },
  async ({ level, method, right }) => {
    ttrace[level].enterMethod(method, right);
    return { content: [{ type: "text", text: `Enter: ${method}` }] };
  }
);

server.tool(
  "tracetool_exit_method",
  "Send an 'Exit methodName' trace with unindent — use at the end of a logical operation.",
  {
    level:   z.enum(["debug", "warning", "error"]).default("debug"),
    method:  z.string().describe("Method or operation name"),
    right:   z.string().optional(),
  },
  async ({ level, method, right }) => {
    ttrace[level].exitMethod(method, right);
    return { content: [{ type: "text", text: `Exit: ${method}` }] };
  }
);

// -----------------------------------------------------------------------
// Tool: get_config
// -----------------------------------------------------------------------
server.tool(
  "tracetool_get_config",
  "Return the current TraceTool runtime configuration: viewer host and the environment detection flags from tracetool.js (isChromeExtension, isBrowser, isNodeJs, isRequireJs, isCommonJS, isSystemJS).",
  {},
  async () => {
    // ttrace.environment exposes the private closure vars as a comma-separated string:
    // "isBrowser:true,isNodeJs:false,isRequireJs:false,isCommonJS:false,isSystemJS:false,isChromeExtension:false"
    
    process.stderr.write(`tracetool_get_config\n`);
    
    const config = { host: ttrace.host };
    for (const pair of ttrace.environment.split(",")) {
      const [key, val] = pair.split(":");
      config[key.trim()] = val.trim() === "true";
    }
    return { content: [{ type: "text", text: JSON.stringify(config, null, 2) }] };
  }
);

// -----------------------------------------------------------------------
// Start
// -----------------------------------------------------------------------
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  // stderr only — stdout is the MCP JSON-RPC channel
  process.stderr.write(`TraceTool MCP server started. Viewer host: ${ttrace.host}\n`);
}

main().catch((err) => {
  process.stderr.write(`Fatal: ${err.message}\n`);
  process.exit(1);
});
