/**
 * Process information and control
 *
 * Node.js-compatible process module.
 * Provides access to process arguments, environment, and system info.
 *
 * @module std/process
 * @example
 * ```typescript
 * import * as process from "std/process";
 *
 * console.log(`Running on ${process.platform}`);
 * console.log(`CWD: ${process.cwd()}`);
 * console.log(`Args: ${process.argv.join(", ")}`);
 *
 * const apiKey = process.env["API_KEY"];
 * ```
 */

// =============================================================================
// Process Arguments
// =============================================================================

/**
 * Command line arguments
 * argv[0] is the executable path
 * argv[1] is the script path
 * argv[2...] are user arguments
 *
 * @extern Populated at program startup
 */
export const argv: string[] = @extern("ms_process_argv");

/**
 * Path to the executable that started the process
 *
 * @extern Maps to: argv[0]
 */
export const execPath: string = @extern("ms_process_exec_path");

// =============================================================================
// Environment
// =============================================================================

/**
 * Environment variables
 *
 * @extern Maps to: std.process.getEnvMap()
 * @example
 * const home = process.env["HOME"];
 * const path = process.env["PATH"];
 */
export const env: Record<string, string> = @extern("ms_process_env");

// =============================================================================
// Working Directory
// =============================================================================

/**
 * Get current working directory
 *
 * @extern Maps to: std.process.getCwd()
 * @example
 * const cwd = process.cwd();
 */
export function cwd(): string {
    @extern("ms_process_cwd");
}

/**
 * Change working directory
 *
 * @extern Maps to: std.posix.chdir()
 * @example
 * process.chdir("/home/user/project");
 */
export function chdir(directory: string): void {
    @extern("ms_process_chdir");
}

// =============================================================================
// Platform Info
// =============================================================================

/**
 * Operating system platform
 * 'darwin' | 'linux' | 'win32' | 'freebsd' | 'openbsd'
 *
 * @extern Compile-time constant based on target
 */
export const platform: string = @extern("ms_process_platform");

/**
 * CPU architecture
 * 'x64' | 'arm64' | 'arm' | 'ia32'
 *
 * @extern Compile-time constant based on target
 */
export const arch: string = @extern("ms_process_arch");

/**
 * Node.js version (for compatibility)
 * Reports Metascript version in Node format
 */
export const version: string = "v20.0.0-metascript";

/**
 * Version details
 */
export const versions: Record<string, string> = {
    metascript: "0.1.0",
    zig: "0.15.0",
    v8: "0.0.0",  // Not applicable
    node: "20.0.0"  // Compatibility version
};

// =============================================================================
// Process Control
// =============================================================================

/**
 * Exit the process with status code
 *
 * @extern Maps to: std.posix.exit()
 * @example
 * if (error) process.exit(1);
 * process.exit(0);  // Success
 */
export function exit(code?: number): never {
    @extern("ms_process_exit");
}

/**
 * Process ID
 *
 * @extern Maps to: std.posix.getpid()
 */
export const pid: number = @extern("ms_process_pid");

/**
 * Parent process ID
 *
 * @extern Maps to: std.posix.getppid()
 */
export const ppid: number = @extern("ms_process_ppid");

// =============================================================================
// Resource Usage
// =============================================================================

/**
 * Memory usage information
 */
export interface MemoryUsage {
    /** Resident Set Size - total memory allocated */
    rss: number;
    /** Total heap allocated */
    heapTotal: number;
    /** Heap actually used */
    heapUsed: number;
    /** Memory used by C++ objects bound to JS */
    external: number;
    /** ArrayBuffers and SharedArrayBuffers */
    arrayBuffers: number;
}

/**
 * Get memory usage statistics
 *
 * @extern Maps to: platform-specific memory APIs
 * @example
 * const mem = process.memoryUsage();
 * console.log(`Heap used: ${mem.heapUsed / 1024 / 1024} MB`);
 */
export function memoryUsage(): MemoryUsage {
    @extern("ms_process_memory_usage");
}

/**
 * CPU usage information
 */
export interface CpuUsage {
    /** Microseconds of user CPU time */
    user: number;
    /** Microseconds of system CPU time */
    system: number;
}

/**
 * Get CPU usage
 *
 * @extern Maps to: getrusage()
 */
export function cpuUsage(previousValue?: CpuUsage): CpuUsage {
    @extern("ms_process_cpu_usage");
}

// =============================================================================
// High Resolution Time
// =============================================================================

/**
 * High-resolution time as [seconds, nanoseconds]
 *
 * @extern Maps to: std.time.nanoTimestamp()
 * @example
 * const start = process.hrtime();
 * doWork();
 * const diff = process.hrtime(start);
 * console.log(`Took ${diff[0]}s ${diff[1]}ns`);
 */
export function hrtime(time?: [number, number]): [number, number] {
    @extern("ms_process_hrtime");
}

/**
 * High-resolution time as bigint nanoseconds
 *
 * @extern Maps to: std.time.nanoTimestamp()
 * @example
 * const start = process.hrtime.bigint();
 * doWork();
 * const elapsed = process.hrtime.bigint() - start;
 */
hrtime.bigint = function(): bigint {
    @extern("ms_process_hrtime_bigint");
};

/**
 * Process uptime in seconds
 *
 * @extern Time since process start
 */
export function uptime(): number {
    @extern("ms_process_uptime");
}

// =============================================================================
// Standard Streams
// =============================================================================

/**
 * Standard input stream
 * TODO: Implement Readable stream
 */
export const stdin: any = @extern("ms_process_stdin");

/**
 * Standard output stream
 * TODO: Implement Writable stream
 */
export const stdout: any = @extern("ms_process_stdout");

/**
 * Standard error stream
 * TODO: Implement Writable stream
 */
export const stderr: any = @extern("ms_process_stderr");

// =============================================================================
// Misc
// =============================================================================

/**
 * Title of the process (as shown in ps)
 */
export let title: string = "metascript";

/**
 * Abort the process (generates core dump)
 *
 * @extern Maps to: std.posix.abort()
 */
export function abort(): never {
    @extern("ms_process_abort");
}

/**
 * Send signal to process
 *
 * @extern Maps to: std.posix.kill()
 */
export function kill(pid: number, signal?: string | number): boolean {
    @extern("ms_process_kill");
}

/**
 * Schedule callback for next tick
 * Note: In compiled code, this executes immediately (no event loop)
 */
export function nextTick(callback: () => void): void {
    callback();
}
