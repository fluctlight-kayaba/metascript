# tree-sitter-metascript

Tree-sitter grammar for Metascript - TypeScript syntax with compile-time macro extensions.

## Features

- ✅ **TypeScript subset** - Classes, functions, types
- ✅ **Macro decorators** - `@derive(Eq, Hash)`, `@serialize`, `@ffi`
- ✅ **Comptime blocks** - `@comptime { ... }`
- ✅ **Full syntax highlighting** - Keywords, types, macros, literals

## Installation

### Neovim (nvim-treesitter)

```lua
-- In your Neovim config
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.metascript = {
  install_info = {
    url = "~/projects/metascript/tree-sitter-metascript",
    files = { "src/parser.c" },
    branch = "main",
  },
  filetype = "metascript",
}

-- Add to your treesitter setup
require('nvim-treesitter.configs').setup {
  ensure_installed = { "metascript" },
  highlight = { enable = true },
}
```

### VS Code

1. Install `vscode-tree-sitter-syntax` extension
2. Add to settings.json:

```json
{
  "treeSitter.grammars": [
    {
      "name": "metascript",
      "path": "~/projects/metascript/tree-sitter-metascript"
    }
  ],
  "files.associations": {
    "*.ms": "metascript"
  }
}
```

## Development

```bash
# Generate parser
npm install
npm run build

# Test grammar
npm test

# Or use tree-sitter CLI directly
tree-sitter generate
tree-sitter test
```

## Syntax Highlighting

The grammar highlights:

- **Keywords**: `class`, `function`, `const`, `let`, `if`, `for`, etc.
- **Macros**: `@derive`, `@comptime`, `@serialize`, `@ffi` (cyan/special color)
- **Types**: `string`, `number`, `boolean`, class names (purple)
- **Functions**: Function/method names (yellow/gold)
- **Literals**: Strings (green), numbers (orange), booleans (orange)
- **Comments**: Gray/dim

## Example

```metascript
@derive(Eq, Hash)
class User {
    name: string;
    age: number;
}

@comptime {
    const config = loadConfig();
    return config.apiUrl;
}

function greet(name: string): void {
    console.log(`Hello, ${name}!`);
}
```

## License

MIT
