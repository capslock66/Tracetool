---
name: tracetool
description: >
  Send structured traces to the TraceTool viewer using the MCP tools exposed by
  the tracetool plugin. Use when the user asks to log, trace, debug, or send
  messages to the TraceTool viewer. Also use when the user asks to clear the viewer,
  inspect an object, or show indented method entry/exit traces.
---

# TraceTool Skill

You have access to a set of `tracetool_*` MCP tools that send structured messages
to the **TraceTool viewer** (a Windows desktop trace viewer).  
The viewer must be running before any tool call will succeed.

## Available tools

| Tool | Purpose |
|---|---|
| `tracetool_set_host` | Change the viewer host (default `127.0.0.1:81`) |
| `tracetool_clear_all` | Clear all traces in the main viewer window |
| `tracetool_send` | Send a debug / warning / error message (1 or 2 columns) |
| `tracetool_indent` | Send a message **and** indent subsequent traces under it |
| `tracetool_unindent` | Close an indent level (optionally with a closing message) |
| `tracetool_enter_method` | `indent` + "Enter " prefix + enter-icon |
| `tracetool_exit_method` | `unindent` + "Exit " prefix + exit-icon |
| `tracetool_send_value` | Send a JSON-serialisable value as an expandable tree |
| `tracetool_send_xml` | Send XML with syntax highlighting |
| `tracetool_send_table` | Send an array of objects as a table |
| `tracetool_get_config` | Return the current host and environment flags |

## How to use

### Simple message
```
tracetool_send(level="debug", message="Hello from Claude")
```

### Two-column trace (left | right)
```
tracetool_send(level="warning", message="Response time", right="450 ms")
```

### Indented block (logical scope)
```
tracetool_enter_method(level="debug", method="ProcessOrder")
tracetool_send(level="debug", message="Validating items")
tracetool_send(level="debug", message="Charging card")
tracetool_exit_method(level="debug", method="ProcessOrder")
```

### Inspect an object
```
tracetool_send_value(level="debug", message="Response", value='{"status":200,"body":"ok"}', maxLevel=3)
```

### Send an XML payload
```
tracetool_send_xml(level="debug", message="SOAP request", xml="<root><item>1</item></root>")
```

### Send a table
```
tracetool_send_table(level="debug", message="Users", rows='[{"id":1,"name":"Alice"},{"id":2,"name":"Bob"}]')
```

## Configuration

- Default host: `127.0.0.1:81`
- Override at runtime: `tracetool_set_host(host="127.0.0.1:85")`
- Override via env variable: set `TRACETOOL_HOST` in the plugin config

## Rules

1. Always use `tracetool_enter_method` / `tracetool_exit_method` in pairs.
2. Always use `tracetool_indent` / `tracetool_unindent` in pairs.
3. Use `level="error"` only for actual errors; `level="warning"` for non-fatal issues.
4. Pass `value` to `tracetool_send_value` as a **JSON string** (call `JSON.stringify` mentally).
5. Pass `rows` to `tracetool_send_table` as a **JSON array string**.
6. If the user has not specified a host, assume `127.0.0.1:81` (already the default).
7. After completing a group of related traces, offer to `tracetool_clear_all` if the user wants a fresh start.
