# Tree-sitter Metascript Testing Guide

## Verification Workflow (No Neovim Required!)

**Always test BEFORE opening Neovim** to catch errors early:

### 1. Grammar Tests
```bash
tree-sitter test
```
✅ Validates parser against test corpus
✅ 100% pass rate = grammar is correct

### 2. Parse Real Files
```bash
tree-sitter parse ../examples/macro.ms
```
✅ Shows actual parse tree
✅ Catches parse errors and ambiguities

### 3. Validate Query Syntax
```bash
tree-sitter query queries/highlights.scm
```
✅ Validates highlight queries
✅ Catches syntax errors BEFORE Neovim
✅ This would have caught the `"@derive"` error immediately!

### 4. Quick Smoke Test
```bash
cat > /tmp/test.ms << 'EOF'
@derive(Eq, Hash)
class User {
    name: string;
}
EOF

tree-sitter parse /tmp/test.ms
```
✅ Fast validation on minimal input

## Common Issues & Solutions

### Issue: String Literals in Queries
**Error**: `"this"` or `"@derive"` in query causes Lua error

**Why**: These are keywords in the source, not node types in the AST

**Solution**: Use node captures instead:
```scheme
; ❌ Wrong - string literals
["this" "@derive"] @keyword

; ✅ Correct - node captures
(this) @variable.builtin
(macro_decorator name: (identifier) @keyword.directive)
```

### Issue: Grammar Node Mismatch
**Error**: Query uses `(property_identifier)` but grammar has `(identifier)`

**Why**: Inheriting from JavaScript/TypeScript uses different node names

**Solution**: Write queries that match YOUR grammar structure:
```bash
# Check actual parse tree structure
tree-sitter parse examples/macro.ms | grep -A2 "property_declaration"
```

### Issue: Type Annotation Structure
**Error**: Query expects `(primitive_type)` but tree has `(type (primitive_type))`

**Why**: Grammar wraps types in `type` node: `type_annotation: $ => seq(':', $.type)`

**Solution**: Match the actual structure:
```scheme
; ❌ Wrong
(type_annotation (primitive_type) @type)

; ✅ Correct
(type_annotation (type (primitive_type)) @type.builtin)
```

## Build & Install

```bash
# Generate parser
npm install
npm run build

# Install in Neovim (automatically done via nvim-treesitter)
# Just open a .ms file and it will install
```

## Current Status

✅ **Grammar**: 100% tests passing
✅ **Parser**: ~99% of examples/macro.ms parses correctly
✅ **Queries**: Valid syntax, all captures match grammar nodes
✅ **Neovim**: Highlighting works perfectly!

## What Gets Highlighted

- **Macro decorators**: `@derive`, `@comptime` → cyan (keyword.directive)
- **Keywords**: `class`, `function`, `const`, `let`, etc. → magenta
- **Types**: `string`, `number`, `boolean` → purple (type.builtin)
- **Class names**: `User`, `Product` → purple (type)
- **Functions**: Function names → yellow/gold
- **Properties**: Class properties → based on theme
- **Literals**: Strings (green), numbers (orange), booleans
- **Comments**: Dim gray

## Files Structure

```
tree-sitter-metascript/
├── grammar.js              # Grammar definition
├── queries/
│   └── highlights.scm     # Syntax highlighting rules
├── test/corpus/
│   └── macros.txt         # Test cases
├── src/
│   └── parser.c           # Generated parser (do not edit)
├── package.json           # npm config
└── TESTING.md            # This file
```

## Debugging Tips

1. **Always run `tree-sitter test` first** - catches grammar issues
2. **Parse actual files** to see real AST structure
3. **Use `tree-sitter query`** to validate queries before Neovim
4. **Check test corpus** - update tests when grammar changes
5. **No inheritance** - we don't inherit from JavaScript to avoid node name mismatches

---

**Key Lesson**: Tree-sitter CLI tools catch errors BEFORE Neovim, saving debugging time!
