/**
 * File System operations
 *
 * Node.js-compatible file system module implemented as compile-time macros.
 * Each function expands to optimal platform-native code - zero runtime overhead.
 *
 * @module std/fs
 * @example
 * ```typescript
 * import * as fs from "std/fs";
 *
 * const content = fs.readFileSync("config.json");
 * fs.writeFileSync("output.txt", "Hello, World!");
 *
 * if (fs.existsSync("data.json")) {
 *     const stats = fs.statSync("data.json");
 *     console.log(`Size: ${stats.size}`);
 * }
 * ```
 */

// =============================================================================
// Types
// =============================================================================

/**
 * File/directory statistics
 */
export interface Stats {
    /** Size in bytes */
    size: number;
    /** Last modification time */
    mtime: Date;
    /** Last access time */
    atime: Date;
    /** Creation time */
    ctime: Date;
    /** File mode/permissions */
    mode: number;

    /** Check if this is a regular file */
    isFile(): boolean;
    /** Check if this is a directory */
    isDirectory(): boolean;
    /** Check if this is a symbolic link */
    isSymbolicLink(): boolean;
}

/**
 * Options for mkdir
 */
export interface MkdirOptions {
    /** Create parent directories as needed */
    recursive?: boolean;
    /** Directory permissions (default: 0o777) */
    mode?: number;
}

/**
 * Options for readdir
 */
export interface ReaddirOptions {
    /** Include file type info */
    withFileTypes?: boolean;
}

/**
 * Directory entry with type info
 */
export interface Dirent {
    name: string;
    isFile(): boolean;
    isDirectory(): boolean;
    isSymbolicLink(): boolean;
}

// =============================================================================
// Read Operations (Macros)
// =============================================================================

/**
 * Read entire file as string (UTF-8)
 *
 * Expands at compile time to platform-native code:
 * - C backend: direct read() syscall with buffer
 * - JS backend: require('fs').readFileSync()
 * - Erlang backend: file:read_file/1
 *
 * @example
 * const content = fs.readFileSync("file.txt");
 */
export macro readFileSync(path: string): string {
    @target("c") {
        // Expands to: ms_read_file_sync(path)
        @emit("ms_read_file_sync($path)")
    }
    @target("js") {
        // Expands to: require('fs').readFileSync(path, 'utf8')
        @emit("require('fs').readFileSync($path, 'utf8')")
    }
    @target("erlang") {
        // Expands to: {ok, Content} = file:read_file(Path), binary_to_list(Content)
        @emit("element(2, file:read_file($path))")
    }
}

/**
 * Read entire file as Buffer (binary)
 *
 * @example
 * const bytes = fs.readFileBinarySync("image.png");
 */
export macro readFileBinarySync(path: string): Buffer {
    @target("c") {
        @emit("ms_read_file_binary_sync($path)")
    }
    @target("js") {
        @emit("require('fs').readFileSync($path)")
    }
    @target("erlang") {
        @emit("element(2, file:read_file($path))")
    }
}

// =============================================================================
// Write Operations (Macros)
// =============================================================================

/**
 * Write data to file, replacing if exists
 *
 * Expands at compile time to platform-native code:
 * - C backend: open() + write() + close()
 * - JS backend: require('fs').writeFileSync()
 * - Erlang backend: file:write_file/2
 *
 * @example
 * fs.writeFileSync("output.txt", "Hello, World!");
 */
export macro writeFileSync(path: string, data: string): void {
    @target("c") {
        @emit("ms_write_file_sync($path, $data)")
    }
    @target("js") {
        @emit("require('fs').writeFileSync($path, $data)")
    }
    @target("erlang") {
        @emit("file:write_file($path, $data)")
    }
}

/**
 * Write binary data to file
 */
export macro writeFileBinarySync(path: string, data: Buffer): void {
    @target("c") {
        @emit("ms_write_file_binary_sync($path, $data)")
    }
    @target("js") {
        @emit("require('fs').writeFileSync($path, $data)")
    }
    @target("erlang") {
        @emit("file:write_file($path, $data)")
    }
}

/**
 * Append data to file
 *
 * @example
 * fs.appendFileSync("log.txt", "New entry\n");
 */
export macro appendFileSync(path: string, data: string): void {
    @target("c") {
        @emit("ms_append_file_sync($path, $data)")
    }
    @target("js") {
        @emit("require('fs').appendFileSync($path, $data)")
    }
    @target("erlang") {
        @emit("file:write_file($path, $data, [append])")
    }
}

// =============================================================================
// File Info Operations (Macros)
// =============================================================================

/**
 * Check if file or directory exists
 *
 * Expands at compile time to platform-native code:
 * - C backend: access() syscall
 * - JS backend: require('fs').existsSync()
 * - Erlang backend: filelib:is_file/1 or filelib:is_dir/1
 *
 * @example
 * if (fs.existsSync("config.json")) {
 *     // file exists
 * }
 */
export macro existsSync(path: string): boolean {
    @target("c") {
        @emit("ms_exists_sync($path)")
    }
    @target("js") {
        @emit("require('fs').existsSync($path)")
    }
    @target("erlang") {
        @emit("filelib:is_file($path) orelse filelib:is_dir($path)")
    }
}

/**
 * Get file/directory statistics
 *
 * @example
 * const stats = fs.statSync("file.txt");
 * console.log(`Size: ${stats.size}, IsDir: ${stats.isDirectory()}`);
 */
export macro statSync(path: string): Stats {
    @target("c") {
        @emit("ms_stat_sync($path)")
    }
    @target("js") {
        @emit("require('fs').statSync($path)")
    }
    @target("erlang") {
        @emit("ms_stat_sync($path)")  // Helper needed for Erlang
    }
}

/**
 * Get statistics without following symlinks
 */
export macro lstatSync(path: string): Stats {
    @target("c") {
        @emit("ms_lstat_sync($path)")
    }
    @target("js") {
        @emit("require('fs').lstatSync($path)")
    }
    @target("erlang") {
        @emit("ms_lstat_sync($path)")
    }
}

// =============================================================================
// Directory Operations (Macros)
// =============================================================================

/**
 * Read directory contents
 *
 * @example
 * const files = fs.readdirSync("./src");
 * for (const file of files) {
 *     console.log(file);
 * }
 */
export macro readdirSync(path: string): string[] {
    @target("c") {
        @emit("ms_readdir_sync($path)")
    }
    @target("js") {
        @emit("require('fs').readdirSync($path)")
    }
    @target("erlang") {
        @emit("element(2, file:list_dir($path))")
    }
}

/**
 * Create directory
 *
 * @example
 * fs.mkdirSync("new-folder");
 */
export macro mkdirSync(path: string): void {
    @target("c") {
        @emit("ms_mkdir_sync($path, 0777, false)")
    }
    @target("js") {
        @emit("require('fs').mkdirSync($path)")
    }
    @target("erlang") {
        @emit("file:make_dir($path)")
    }
}

/**
 * Create directory with options
 *
 * @example
 * fs.mkdirSyncRecursive("deeply/nested/path");
 */
export macro mkdirSyncRecursive(path: string): void {
    @target("c") {
        @emit("ms_mkdir_sync($path, 0777, true)")
    }
    @target("js") {
        @emit("require('fs').mkdirSync($path, { recursive: true })")
    }
    @target("erlang") {
        @emit("filelib:ensure_dir($path ++ \"/\")")
    }
}

/**
 * Remove empty directory
 *
 * @example
 * fs.rmdirSync("empty-folder");
 */
export macro rmdirSync(path: string): void {
    @target("c") {
        @emit("ms_rmdir_sync($path)")
    }
    @target("js") {
        @emit("require('fs').rmdirSync($path)")
    }
    @target("erlang") {
        @emit("file:del_dir($path)")
    }
}

/**
 * Remove file or directory recursively
 *
 * @example
 * fs.rmSync("folder");
 */
export macro rmSync(path: string): void {
    @target("c") {
        @emit("ms_rm_sync($path, true)")
    }
    @target("js") {
        @emit("require('fs').rmSync($path, { recursive: true, force: true })")
    }
    @target("erlang") {
        @emit("ms_rm_recursive($path)")
    }
}

// =============================================================================
// File Operations (Macros)
// =============================================================================

/**
 * Delete file
 *
 * @example
 * fs.unlinkSync("temp.txt");
 */
export macro unlinkSync(path: string): void {
    @target("c") {
        @emit("ms_unlink_sync($path)")
    }
    @target("js") {
        @emit("require('fs').unlinkSync($path)")
    }
    @target("erlang") {
        @emit("file:delete($path)")
    }
}

/**
 * Rename/move file or directory
 *
 * @example
 * fs.renameSync("old.txt", "new.txt");
 */
export macro renameSync(oldPath: string, newPath: string): void {
    @target("c") {
        @emit("ms_rename_sync($oldPath, $newPath)")
    }
    @target("js") {
        @emit("require('fs').renameSync($oldPath, $newPath)")
    }
    @target("erlang") {
        @emit("file:rename($oldPath, $newPath)")
    }
}

/**
 * Copy file
 *
 * @example
 * fs.copyFileSync("source.txt", "dest.txt");
 */
export macro copyFileSync(src: string, dest: string): void {
    @target("c") {
        @emit("ms_copy_file_sync($src, $dest)")
    }
    @target("js") {
        @emit("require('fs').copyFileSync($src, $dest)")
    }
    @target("erlang") {
        @emit("file:copy($src, $dest)")
    }
}

// =============================================================================
// Symlink Operations (Macros)
// =============================================================================

/**
 * Create symbolic link
 */
export macro symlinkSync(target: string, path: string): void {
    @target("c") {
        @emit("ms_symlink_sync($target, $path)")
    }
    @target("js") {
        @emit("require('fs').symlinkSync($target, $path)")
    }
    @target("erlang") {
        @emit("file:make_symlink($target, $path)")
    }
}

/**
 * Read symbolic link target
 */
export macro readlinkSync(path: string): string {
    @target("c") {
        @emit("ms_readlink_sync($path)")
    }
    @target("js") {
        @emit("require('fs').readlinkSync($path)")
    }
    @target("erlang") {
        @emit("element(2, file:read_link($path))")
    }
}

/**
 * Get real/canonical path (resolving symlinks)
 */
export macro realpathSync(path: string): string {
    @target("c") {
        @emit("ms_realpath_sync($path)")
    }
    @target("js") {
        @emit("require('fs').realpathSync($path)")
    }
    @target("erlang") {
        @emit("filename:absname($path)")  // Note: doesn't fully resolve symlinks
    }
}

// =============================================================================
// Permission Operations (Macros)
// =============================================================================

/**
 * Change file permissions
 *
 * @example
 * fs.chmodSync("script.sh", 0o755);
 */
export macro chmodSync(path: string, mode: number): void {
    @target("c") {
        @emit("ms_chmod_sync($path, $mode)")
    }
    @target("js") {
        @emit("require('fs').chmodSync($path, $mode)")
    }
    @target("erlang") {
        @emit("file:change_mode($path, $mode)")
    }
}

/**
 * Change file owner
 */
export macro chownSync(path: string, uid: number, gid: number): void {
    @target("c") {
        @emit("ms_chown_sync($path, $uid, $gid)")
    }
    @target("js") {
        @emit("require('fs').chownSync($path, $uid, $gid)")
    }
    @target("erlang") {
        @emit("file:change_owner($path, $uid, $gid)")
    }
}

// =============================================================================
// Watch Operations (Macros)
// =============================================================================

/**
 * Watch file for changes
 *
 * Platform implementations:
 * - C: inotify (Linux), FSEvents (macOS), ReadDirectoryChangesW (Windows)
 * - JS: fs.watchFile
 * - Erlang: filelib:file_modified_time polling
 */
export macro watchFile(path: string, listener: (curr: Stats, prev: Stats) => void): void {
    @target("c") {
        @emit("ms_watch_file($path, $listener)")
    }
    @target("js") {
        @emit("require('fs').watchFile($path, $listener)")
    }
    @target("erlang") {
        @emit("ms_watch_file($path, $listener)")
    }
}

/**
 * Stop watching file
 */
export macro unwatchFile(path: string): void {
    @target("c") {
        @emit("ms_unwatch_file($path)")
    }
    @target("js") {
        @emit("require('fs').unwatchFile($path)")
    }
    @target("erlang") {
        @emit("ms_unwatch_file($path)")
    }
}
