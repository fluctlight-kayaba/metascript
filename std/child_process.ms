/**
 * Child process spawning and management
 *
 * Node.js-compatible child_process module for running external commands.
 * Maps directly to Zig std.process.Child - no libuv overhead.
 *
 * @module std/child_process
 * @example
 * ```typescript
 * import { spawn, execSync } from "std/child_process";
 *
 * // Spawn async process
 * const child = spawn("ls", ["-la"]);
 * child.stdout.on("data", (data) => console.log(data));
 * child.on("close", (code) => console.log(`Exit: ${code}`));
 *
 * // Sync execution
 * const output = execSync("git status");
 * console.log(output);
 * ```
 */

import { EventEmitter } from "std/events";

// =============================================================================
// Types
// =============================================================================

/**
 * Standard I/O configuration for child process
 *
 * @description
 * - `'pipe'` - Create pipe between parent and child (default)
 * - `'inherit'` - Child uses parent's stdio
 * - `'ignore'` - Discard the stream
 */
export type StdioOption = "pipe" | "inherit" | "ignore";

/**
 * Options for spawn()
 */
export interface SpawnOptions {
    /**
     * Current working directory for the child process
     * @default process.cwd()
     */
    cwd?: string;

    /**
     * Environment variables for the child process
     * @default process.env
     */
    env?: Record<string, string>;

    /**
     * Standard I/O configuration
     * Can be a single value or array [stdin, stdout, stderr]
     * @default 'pipe'
     */
    stdio?: StdioOption | [StdioOption, StdioOption, StdioOption];

    /**
     * Run command in shell
     * @default false
     */
    shell?: boolean | string;

    /**
     * Timeout in milliseconds (0 = no timeout)
     * @default 0
     */
    timeout?: number;

    /**
     * Detach the child process from parent
     * @default false
     */
    detached?: boolean;

    /**
     * User ID to run as (Unix only)
     */
    uid?: number;

    /**
     * Group ID to run as (Unix only)
     */
    gid?: number;
}

/**
 * Options for execSync()
 */
export interface ExecSyncOptions {
    /**
     * Current working directory
     */
    cwd?: string;

    /**
     * Environment variables
     */
    env?: Record<string, string>;

    /**
     * Timeout in milliseconds
     * @default 0 (no timeout)
     */
    timeout?: number;

    /**
     * Maximum buffer size for stdout/stderr
     * @default 1024 * 1024 (1MB)
     */
    maxBuffer?: number;

    /**
     * Encoding for returned string
     * @default 'utf8'
     */
    encoding?: string;

    /**
     * Shell to execute command in
     * @default '/bin/sh' on Unix, 'cmd.exe' on Windows
     */
    shell?: string;
}

/**
 * Result of spawnSync()
 */
export interface SpawnSyncResult {
    /**
     * Process ID (may be undefined if spawn failed)
     */
    pid?: number;

    /**
     * Stdout as string or Buffer
     */
    stdout: string | Buffer;

    /**
     * Stderr as string or Buffer
     */
    stderr: string | Buffer;

    /**
     * Exit code (null if terminated by signal)
     */
    status: number | null;

    /**
     * Signal that terminated the process (null if exited normally)
     */
    signal: string | null;

    /**
     * Error if spawn failed
     */
    error?: Error;
}

// =============================================================================
// Readable/Writable stream placeholders
// =============================================================================

/**
 * Readable stream for stdout/stderr
 * TODO: Implement full stream interface
 */
export interface Readable extends EventEmitter {
    /**
     * Read all available data
     */
    read(): string | null;

    /**
     * Set encoding for string output
     */
    setEncoding(encoding: string): this;

    /**
     * Pause the stream
     */
    pause(): this;

    /**
     * Resume the stream
     */
    resume(): this;

    /**
     * Pipe to another stream
     */
    pipe<T extends Writable>(destination: T): T;
}

/**
 * Writable stream for stdin
 * TODO: Implement full stream interface
 */
export interface Writable extends EventEmitter {
    /**
     * Write data to stream
     */
    write(chunk: string | Buffer): boolean;

    /**
     * End the stream
     */
    end(chunk?: string | Buffer): this;

    /**
     * Set default encoding
     */
    setDefaultEncoding(encoding: string): this;
}

// =============================================================================
// ChildProcess class
// =============================================================================

/**
 * Represents a spawned child process
 *
 * @description
 * Extends EventEmitter and provides access to the child's stdin, stdout, stderr.
 *
 * @fires close - Emitted when process exits and stdio streams are closed
 * @fires exit - Emitted when process exits (code, signal)
 * @fires error - Emitted on spawn error
 * @fires spawn - Emitted when process is successfully spawned
 *
 * @example
 * ```typescript
 * const child = spawn("node", ["script.js"]);
 *
 * child.stdout.on("data", (data) => {
 *     console.log(`stdout: ${data}`);
 * });
 *
 * child.stderr.on("data", (data) => {
 *     console.error(`stderr: ${data}`);
 * });
 *
 * child.on("close", (code) => {
 *     console.log(`child process exited with code ${code}`);
 * });
 * ```
 */
export class ChildProcess extends EventEmitter {
    /**
     * Process ID
     * @readonly
     */
    pid: number;

    /**
     * Standard input stream (writable)
     * null if stdio[0] is not 'pipe'
     */
    stdin: Writable | null;

    /**
     * Standard output stream (readable)
     * null if stdio[1] is not 'pipe'
     */
    stdout: Readable | null;

    /**
     * Standard error stream (readable)
     * null if stdio[2] is not 'pipe'
     */
    stderr: Readable | null;

    /**
     * Whether the process is connected (IPC channel open)
     */
    connected: boolean;

    /**
     * Exit code (null until process exits)
     */
    exitCode: number | null;

    /**
     * Signal that terminated the process (null if exited normally)
     */
    signalCode: string | null;

    /**
     * Whether the child process was killed
     */
    killed: boolean;

    /**
     * Whether spawn was successful
     */
    spawnfile: string;

    /**
     * Arguments passed to spawn
     */
    spawnargs: string[];

    /**
     * @internal
     */
    constructor() {
        super();
        this.pid = 0;
        this.stdin = null;
        this.stdout = null;
        this.stderr = null;
        this.connected = false;
        this.exitCode = null;
        this.signalCode = null;
        this.killed = false;
        this.spawnfile = "";
        this.spawnargs = [];
    }

    /**
     * Send signal to child process
     *
     * @param signal - Signal to send (default: 'SIGTERM')
     * @returns true if signal was sent successfully
     *
     * @example
     * ```typescript
     * child.kill();          // Send SIGTERM
     * child.kill("SIGKILL"); // Force kill
     * child.kill("SIGINT");  // Interrupt (Ctrl+C)
     * ```
     */
    kill(signal?: string): boolean {
        @extern("ms_child_process_kill");
    }

    /**
     * Send message to child (requires IPC channel)
     *
     * @param message - Message to send (will be serialized)
     * @returns true if message was sent
     */
    send(message: any): boolean {
        @extern("ms_child_process_send");
    }

    /**
     * Close IPC channel
     */
    disconnect(): void {
        this.connected = false;
        @extern("ms_child_process_disconnect");
    }

    /**
     * Reference the child so it keeps the event loop alive
     */
    ref(): this {
        return this;
    }

    /**
     * Unreference the child so it doesn't keep event loop alive
     */
    unref(): this {
        return this;
    }
}

// =============================================================================
// Spawn functions
// =============================================================================

/**
 * Spawn a new process asynchronously
 *
 * @param command - Command to run
 * @param args - Arguments to pass to command
 * @param options - Spawn options
 * @returns ChildProcess instance
 *
 * @description
 * Launches a new process with the given command. The returned ChildProcess
 * is an EventEmitter that allows you to monitor the process.
 *
 * @extern Maps to: std.process.Child.spawn()
 *
 * @example
 * ```typescript
 * // Simple command
 * const ls = spawn("ls", ["-la"]);
 *
 * // With options
 * const node = spawn("node", ["app.js"], {
 *     cwd: "/path/to/project",
 *     env: { NODE_ENV: "production" }
 * });
 *
 * // With shell
 * const cmd = spawn("echo $HOME", [], { shell: true });
 * ```
 */
export function spawn(
    command: string,
    args?: string[],
    options?: SpawnOptions
): ChildProcess {
    @extern("ms_child_process_spawn");
}

/**
 * Spawn a process and wait for completion synchronously
 *
 * @param command - Command to run
 * @param args - Arguments to pass
 * @param options - Spawn options
 * @returns Object with stdout, stderr, status, and signal
 *
 * @extern Maps to: std.process.Child.spawn() + wait()
 *
 * @example
 * ```typescript
 * const result = spawnSync("git", ["status"]);
 * if (result.status === 0) {
 *     console.log(result.stdout.toString());
 * } else {
 *     console.error(result.stderr.toString());
 * }
 * ```
 */
export function spawnSync(
    command: string,
    args?: string[],
    options?: SpawnOptions
): SpawnSyncResult {
    @extern("ms_child_process_spawn_sync");
}

/**
 * Execute command in shell and return output synchronously
 *
 * @param command - Full command string (run in shell)
 * @param options - Execution options
 * @returns stdout as string
 * @throws Error if command fails (non-zero exit)
 *
 * @description
 * Runs command in a shell and buffers the output. Throws if command
 * exits with non-zero code.
 *
 * @extern Maps to: spawn with shell + wait + collect output
 *
 * @example
 * ```typescript
 * try {
 *     const output = execSync("git log --oneline -5");
 *     console.log(output);
 * } catch (error) {
 *     console.error(`Command failed: ${error.message}`);
 * }
 * ```
 */
export function execSync(command: string, options?: ExecSyncOptions): string {
    @extern("ms_child_process_exec_sync");
}

/**
 * Execute command asynchronously
 *
 * @param command - Full command string (run in shell)
 * @param options - Execution options
 * @param callback - Called with (error, stdout, stderr)
 * @returns ChildProcess instance
 *
 * @example
 * ```typescript
 * exec("ls -la", (error, stdout, stderr) => {
 *     if (error) {
 *         console.error(`Error: ${error.message}`);
 *         return;
 *     }
 *     console.log(stdout);
 * });
 * ```
 */
export function exec(
    command: string,
    options?: ExecSyncOptions,
    callback?: (error: Error | null, stdout: string, stderr: string) => void
): ChildProcess {
    @extern("ms_child_process_exec");
}

/**
 * Execute file directly (not through shell)
 *
 * @param file - Path to executable
 * @param args - Arguments
 * @param options - Execution options
 * @param callback - Called with (error, stdout, stderr)
 * @returns ChildProcess instance
 *
 * @example
 * ```typescript
 * execFile("/usr/bin/node", ["--version"], (error, stdout) => {
 *     console.log(`Node version: ${stdout.trim()}`);
 * });
 * ```
 */
export function execFile(
    file: string,
    args?: string[],
    options?: ExecSyncOptions,
    callback?: (error: Error | null, stdout: string, stderr: string) => void
): ChildProcess {
    @extern("ms_child_process_exec_file");
}

/**
 * Execute file synchronously
 *
 * @param file - Path to executable
 * @param args - Arguments
 * @param options - Execution options
 * @returns stdout as string
 */
export function execFileSync(
    file: string,
    args?: string[],
    options?: ExecSyncOptions
): string {
    @extern("ms_child_process_exec_file_sync");
}

/**
 * Fork a new Metascript process with IPC channel
 *
 * @param modulePath - Path to module to run
 * @param args - Arguments to pass
 * @param options - Fork options
 * @returns ChildProcess with IPC channel
 *
 * @description
 * Similar to spawn but sets up an IPC channel for parent-child communication.
 *
 * @example
 * ```typescript
 * const child = fork("worker.ms");
 *
 * child.on("message", (msg) => {
 *     console.log(`Message from child: ${msg}`);
 * });
 *
 * child.send({ type: "start", data: [1, 2, 3] });
 * ```
 */
export function fork(
    modulePath: string,
    args?: string[],
    options?: SpawnOptions
): ChildProcess {
    @extern("ms_child_process_fork");
}
