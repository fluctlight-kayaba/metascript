// Simple TypeScript LSP plugin
// Requires: npm install -g typescript-language-server typescript

// Start LSP when TypeScript or JavaScript files are opened
vim.autocmd.create('FileType', {
  pattern: 'typescript,typescriptreact,javascript,javascriptreact',
  callback: function(ev: { match?: string }) {
    const bufnr = vim.api.getCurrentBuf();

    vim.lsp.start({
      name: 'tsserver',
      cmd: ['typescript-language-server', '--stdio'],
      rootDir: vim.fn.getCwd(),
    });
  }
});

// Keymaps for LSP actions
vim.keymap.set('n', 'K', function() { vim.lsp.buf.hover(); });
vim.keymap.set('n', 'gd', function() { vim.lsp.buf.definition(); });
vim.keymap.set('n', 'gr', function() { vim.lsp.buf.references(); });
