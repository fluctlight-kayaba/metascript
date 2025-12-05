/**
 * Operating system utilities
 *
 * Node.js-compatible os module for system information.
 *
 * @module std/os
 * @example
 * ```typescript
 * import * as os from "std/os";
 *
 * console.log(`Platform: ${os.platform()}`);
 * console.log(`Home: ${os.homedir()}`);
 * console.log(`CPUs: ${os.cpus().length}`);
 * console.log(`Free memory: ${os.freemem() / 1024 / 1024} MB`);
 * ```
 */

// =============================================================================
// Platform Info
// =============================================================================

/**
 * Get operating system platform
 *
 * @returns Platform identifier
 *
 * @description
 * Returns one of: 'darwin', 'linux', 'win32', 'freebsd', 'openbsd', 'sunos'
 *
 * @extern Compile-time constant
 *
 * @example
 * ```typescript
 * if (os.platform() === "darwin") {
 *     console.log("Running on macOS");
 * }
 * ```
 */
export function platform(): string {
    @extern("ms_os_platform");
}

/**
 * Get operating system type
 *
 * @returns OS type name
 *
 * @description
 * Returns: 'Linux', 'Darwin', 'Windows_NT', etc.
 *
 * @extern Maps to: uname().sysname
 */
export function type(): string {
    @extern("ms_os_type");
}

/**
 * Get OS release version
 *
 * @returns Release version string
 *
 * @extern Maps to: uname().release
 *
 * @example
 * ```typescript
 * os.release();  // e.g., "22.1.0" on macOS
 * ```
 */
export function release(): string {
    @extern("ms_os_release");
}

/**
 * Get OS version
 *
 * @returns Version string
 *
 * @extern Maps to: uname().version
 */
export function version(): string {
    @extern("ms_os_version");
}

/**
 * Get CPU architecture
 *
 * @returns Architecture identifier
 *
 * @description
 * Returns: 'x64', 'arm64', 'arm', 'ia32', etc.
 *
 * @extern Compile-time constant
 */
export function arch(): string {
    @extern("ms_os_arch");
}

/**
 * Get machine type
 *
 * @returns Machine identifier (e.g., 'arm64', 'x86_64')
 *
 * @extern Maps to: uname().machine
 */
export function machine(): string {
    @extern("ms_os_machine");
}

/**
 * Get system endianness
 *
 * @returns 'BE' for big endian, 'LE' for little endian
 *
 * @extern Compile-time constant
 */
export function endianness(): "BE" | "LE" {
    @extern("ms_os_endianness");
}

// =============================================================================
// Directories
// =============================================================================

/**
 * Get current user's home directory
 *
 * @returns Home directory path
 *
 * @extern Maps to: $HOME or platform equivalent
 *
 * @example
 * ```typescript
 * const configPath = path.join(os.homedir(), ".config", "myapp");
 * ```
 */
export function homedir(): string {
    @extern("ms_os_homedir");
}

/**
 * Get system temporary directory
 *
 * @returns Temp directory path
 *
 * @extern Maps to: $TMPDIR or /tmp
 *
 * @example
 * ```typescript
 * const tempFile = path.join(os.tmpdir(), "myapp-temp.txt");
 * ```
 */
export function tmpdir(): string {
    @extern("ms_os_tmpdir");
}

// =============================================================================
// User Info
// =============================================================================

/**
 * User information
 */
export interface UserInfo {
    /** Username */
    username: string;
    /** User ID (Unix) */
    uid: number;
    /** Group ID (Unix) */
    gid: number;
    /** User shell */
    shell: string;
    /** Home directory */
    homedir: string;
}

/**
 * Get current user information
 *
 * @returns User info object
 *
 * @extern Maps to: getpwuid(getuid())
 *
 * @example
 * ```typescript
 * const user = os.userInfo();
 * console.log(`Hello, ${user.username}!`);
 * ```
 */
export function userInfo(): UserInfo {
    @extern("ms_os_userinfo");
}

/**
 * Get system hostname
 *
 * @returns Hostname string
 *
 * @extern Maps to: gethostname()
 */
export function hostname(): string {
    @extern("ms_os_hostname");
}

// =============================================================================
// CPU Info
// =============================================================================

/**
 * CPU core information
 */
export interface CpuInfo {
    /** CPU model name */
    model: string;
    /** Clock speed in MHz */
    speed: number;
    /** Time spent in different modes */
    times: {
        user: number;
        nice: number;
        sys: number;
        idle: number;
        irq: number;
    };
}

/**
 * Get information about each CPU core
 *
 * @returns Array of CPU info objects
 *
 * @extern Maps to: platform-specific CPU APIs
 *
 * @example
 * ```typescript
 * const cores = os.cpus();
 * console.log(`${cores.length} CPU cores`);
 * console.log(`Model: ${cores[0].model}`);
 * ```
 */
export function cpus(): CpuInfo[] {
    @extern("ms_os_cpus");
}

// =============================================================================
// Memory Info
// =============================================================================

/**
 * Get total system memory in bytes
 *
 * @returns Total memory in bytes
 *
 * @extern Maps to: sysinfo().totalram or equivalent
 *
 * @example
 * ```typescript
 * const totalGB = os.totalmem() / 1024 / 1024 / 1024;
 * console.log(`Total RAM: ${totalGB.toFixed(1)} GB`);
 * ```
 */
export function totalmem(): number {
    @extern("ms_os_totalmem");
}

/**
 * Get free system memory in bytes
 *
 * @returns Free memory in bytes
 *
 * @extern Maps to: sysinfo().freeram or equivalent
 *
 * @example
 * ```typescript
 * const freeGB = os.freemem() / 1024 / 1024 / 1024;
 * console.log(`Free RAM: ${freeGB.toFixed(1)} GB`);
 * ```
 */
export function freemem(): number {
    @extern("ms_os_freemem");
}

// =============================================================================
// Load & Uptime
// =============================================================================

/**
 * Get system load averages (1, 5, 15 minutes)
 *
 * @returns Array of 3 load average values
 *
 * @description
 * Returns [0, 0, 0] on Windows (not supported).
 *
 * @extern Maps to: getloadavg()
 *
 * @example
 * ```typescript
 * const [load1, load5, load15] = os.loadavg();
 * console.log(`Load average: ${load1.toFixed(2)}`);
 * ```
 */
export function loadavg(): [number, number, number] {
    @extern("ms_os_loadavg");
}

/**
 * Get system uptime in seconds
 *
 * @returns Uptime in seconds
 *
 * @extern Maps to: sysinfo().uptime or equivalent
 *
 * @example
 * ```typescript
 * const days = os.uptime() / 60 / 60 / 24;
 * console.log(`System up for ${days.toFixed(1)} days`);
 * ```
 */
export function uptime(): number {
    @extern("ms_os_uptime");
}

// =============================================================================
// Network
// =============================================================================

/**
 * Network interface information
 */
export interface NetworkInterfaceInfo {
    /** IP address */
    address: string;
    /** Netmask */
    netmask: string;
    /** IP family: 'IPv4' or 'IPv6' */
    family: "IPv4" | "IPv6";
    /** MAC address */
    mac: string;
    /** Is internal/loopback */
    internal: boolean;
    /** CIDR notation */
    cidr: string | null;
    /** Scope ID (IPv6 only) */
    scopeid?: number;
}

/**
 * Get network interfaces
 *
 * @returns Map of interface name to array of addresses
 *
 * @extern Maps to: getifaddrs()
 *
 * @example
 * ```typescript
 * const interfaces = os.networkInterfaces();
 * for (const [name, addrs] of Object.entries(interfaces)) {
 *     for (const addr of addrs) {
 *         if (addr.family === "IPv4" && !addr.internal) {
 *             console.log(`${name}: ${addr.address}`);
 *         }
 *     }
 * }
 * ```
 */
export function networkInterfaces(): Record<string, NetworkInterfaceInfo[]> {
    @extern("ms_os_network_interfaces");
}

// =============================================================================
// Constants
// =============================================================================

/**
 * OS constants (signals, error codes, etc.)
 */
export const constants = {
    signals: {
        SIGHUP: 1,
        SIGINT: 2,
        SIGQUIT: 3,
        SIGILL: 4,
        SIGTRAP: 5,
        SIGABRT: 6,
        SIGBUS: 7,
        SIGFPE: 8,
        SIGKILL: 9,
        SIGUSR1: 10,
        SIGSEGV: 11,
        SIGUSR2: 12,
        SIGPIPE: 13,
        SIGALRM: 14,
        SIGTERM: 15,
        SIGCHLD: 17,
        SIGCONT: 18,
        SIGSTOP: 19,
        SIGTSTP: 20,
        SIGTTIN: 21,
        SIGTTOU: 22,
        SIGURG: 23,
        SIGXCPU: 24,
        SIGXFSZ: 25,
        SIGVTALRM: 26,
        SIGPROF: 27,
        SIGWINCH: 28,
        SIGIO: 29,
        SIGPWR: 30,
        SIGSYS: 31
    },
    errno: {
        E2BIG: 7,
        EACCES: 13,
        EADDRINUSE: 48,
        EADDRNOTAVAIL: 49,
        EAFNOSUPPORT: 47,
        EAGAIN: 35,
        EALREADY: 37,
        EBADF: 9,
        EBADMSG: 94,
        EBUSY: 16,
        ECANCELED: 89,
        ECHILD: 10,
        ECONNABORTED: 53,
        ECONNREFUSED: 61,
        ECONNRESET: 54,
        EDEADLK: 11,
        EDESTADDRREQ: 39,
        EDOM: 33,
        EEXIST: 17,
        EFAULT: 14,
        EFBIG: 27,
        EHOSTUNREACH: 65,
        EINPROGRESS: 36,
        EINTR: 4,
        EINVAL: 22,
        EIO: 5,
        EISCONN: 56,
        EISDIR: 21,
        ELOOP: 62,
        EMFILE: 24,
        EMSGSIZE: 40,
        ENAMETOOLONG: 63,
        ENETDOWN: 50,
        ENETRESET: 52,
        ENETUNREACH: 51,
        ENFILE: 23,
        ENOBUFS: 55,
        ENODEV: 19,
        ENOENT: 2,
        ENOMEM: 12,
        ENOPROTOOPT: 42,
        ENOSPC: 28,
        ENOSYS: 78,
        ENOTCONN: 57,
        ENOTDIR: 20,
        ENOTEMPTY: 66,
        ENOTSOCK: 38,
        ENOTSUP: 45,
        ENOTTY: 25,
        ENXIO: 6,
        EOPNOTSUPP: 102,
        EOVERFLOW: 84,
        EPERM: 1,
        EPIPE: 32,
        EPROTO: 100,
        EPROTONOSUPPORT: 43,
        EPROTOTYPE: 41,
        ERANGE: 34,
        EROFS: 30,
        ESPIPE: 29,
        ESRCH: 3,
        ETIMEDOUT: 60,
        ETXTBSY: 26,
        EWOULDBLOCK: 35,
        EXDEV: 18
    }
};

/**
 * End-of-line marker for current platform
 */
export const EOL: string = platform() === "win32" ? "\r\n" : "\n";

/**
 * Path to /dev/null or equivalent
 */
export const devNull: string = platform() === "win32" ? "\\\\.\\nul" : "/dev/null";
