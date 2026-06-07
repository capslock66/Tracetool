# TraceTool — Claude Code Plugin

Send structured traces to the [TraceTool](https://github.com/capslock66/Tracetool) viewer directly from Claude Code.

## Prerequisites

- **TraceTool viewer** running on Windows (default port 81 for HTTP)
- **Node.js** installed

## Installation

```bash
claude plugin install tracetool@<marketplace>
```

Or for local use:
```bash
claude --plugin-dir ./ClaudePlugin
```

## Configuration

The viewer host defaults to `127.0.0.1:81`. Override it via the plugin config or at runtime:

```json
{
  "pluginConfigs": {
    "tracetool@<marketplace>": {
      "mcpServers": {
        "tracetool": {
          "env": { "TRACETOOL_HOST": "127.0.0.1:85" }
        }
      }
    }
  }
}
```

Or call the tool directly:
> "set tracetool host to 127.0.0.1:85"

## Available tools

| Tool | Description |
|---|---|
| `tracetool_send` | Send a debug / warning / error message |
| `tracetool_clear_all` | Clear the viewer |
| `tracetool_indent` / `tracetool_unindent` | Indented scope |
| `tracetool_enter_method` / `tracetool_exit_method` | Method entry/exit with icons |
| `tracetool_send_value` | Expandable object tree |
| `tracetool_send_xml` | XML with syntax highlighting |
| `tracetool_send_table` | Multi-column table |
| `tracetool_get_config` | Current host + environment flags |
| `tracetool_set_host` | Change viewer host at runtime |

## Usage examples

Just ask Claude naturally:

- *"Send a debug trace: processing started"*
- *"Log this object to TraceTool"* (and paste a JSON)
- *"Clear the viewer and trace the login flow"*
- *"Enter method ProcessOrder in TraceTool"*
