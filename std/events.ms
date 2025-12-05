/**
 * Event emitter pattern
 *
 * Node.js-compatible EventEmitter for pub/sub patterns.
 * Pure implementation - no external dependencies.
 *
 * @module std/events
 * @example
 * ```typescript
 * import { EventEmitter } from "std/events";
 *
 * const emitter = new EventEmitter();
 *
 * emitter.on("data", (msg) => {
 *     console.log(`Received: ${msg}`);
 * });
 *
 * emitter.emit("data", "Hello!");
 * ```
 */

/**
 * Event listener function type
 */
export type Listener = (...args: any[]) => void;

/**
 * Internal listener wrapper to track once listeners
 */
interface ListenerEntry {
    fn: Listener;
    once: boolean;
}

/**
 * EventEmitter class
 *
 * Provides publish/subscribe pattern for decoupled communication.
 */
export class EventEmitter {
    private _events: Map<string, ListenerEntry[]>;
    private _maxListeners: number;

    constructor() {
        this._events = new Map();
        this._maxListeners = 10;
    }

    /**
     * Add event listener
     *
     * @example
     * emitter.on("message", (data) => console.log(data));
     */
    on(event: string, listener: Listener): this {
        return this.addListener(event, listener);
    }

    /**
     * Add event listener (alias for on)
     */
    addListener(event: string, listener: Listener): this {
        let listeners = this._events.get(event);
        if (!listeners) {
            listeners = [];
            this._events.set(event, listeners);
        }

        // Warn if too many listeners (possible memory leak)
        if (listeners.length >= this._maxListeners && this._maxListeners > 0) {
            console.warn(
                `MaxListenersExceededWarning: Possible memory leak detected. ` +
                `${listeners.length + 1} ${event} listeners added. ` +
                `Use emitter.setMaxListeners() to increase limit.`
            );
        }

        listeners.push({ fn: listener, once: false });

        return this;
    }

    /**
     * Add one-time listener (automatically removed after first emit)
     *
     * @example
     * emitter.once("connect", () => console.log("Connected!"));
     */
    once(event: string, listener: Listener): this {
        let listeners = this._events.get(event);
        if (!listeners) {
            listeners = [];
            this._events.set(event, listeners);
        }

        listeners.push({ fn: listener, once: true });

        return this;
    }

    /**
     * Remove event listener
     *
     * @example
     * const handler = (data) => console.log(data);
     * emitter.on("message", handler);
     * emitter.off("message", handler);
     */
    off(event: string, listener: Listener): this {
        return this.removeListener(event, listener);
    }

    /**
     * Remove event listener (alias for off)
     */
    removeListener(event: string, listener: Listener): this {
        const listeners = this._events.get(event);
        if (!listeners) return this;

        const index = listeners.findIndex(entry => entry.fn === listener);
        if (index !== -1) {
            listeners.splice(index, 1);
        }

        if (listeners.length === 0) {
            this._events.delete(event);
        }

        return this;
    }

    /**
     * Remove all listeners for event (or all events if no event specified)
     *
     * @example
     * emitter.removeAllListeners("message");
     * emitter.removeAllListeners();  // Remove all
     */
    removeAllListeners(event?: string): this {
        if (event !== undefined) {
            this._events.delete(event);
        } else {
            this._events.clear();
        }
        return this;
    }

    /**
     * Emit event to all listeners
     *
     * @returns true if event had listeners, false otherwise
     * @example
     * emitter.emit("message", "Hello", { from: "user" });
     */
    emit(event: string, ...args: any[]): boolean {
        const listeners = this._events.get(event);
        if (!listeners || listeners.length === 0) {
            // Special handling for 'error' event
            if (event === "error") {
                const err = args[0];
                if (err instanceof Error) {
                    throw err;
                }
                throw new Error(`Unhandled error: ${err}`);
            }
            return false;
        }

        // Create a copy to allow modifications during iteration
        const toCall = [...listeners];

        // Remove once listeners before calling
        const remaining = listeners.filter(entry => !entry.once);
        if (remaining.length === 0) {
            this._events.delete(event);
        } else if (remaining.length !== listeners.length) {
            this._events.set(event, remaining);
        }

        // Call all listeners
        for (const entry of toCall) {
            entry.fn.apply(this, args);
        }

        return true;
    }

    /**
     * Get number of listeners for event
     *
     * @example
     * const count = emitter.listenerCount("message");
     */
    listenerCount(event: string): number {
        const listeners = this._events.get(event);
        return listeners ? listeners.length : 0;
    }

    /**
     * Get copy of listeners array for event
     *
     * @example
     * const handlers = emitter.listeners("message");
     */
    listeners(event: string): Listener[] {
        const listeners = this._events.get(event);
        if (!listeners) return [];
        return listeners.map(entry => entry.fn);
    }

    /**
     * Get copy of listeners including once wrappers
     */
    rawListeners(event: string): Listener[] {
        return this.listeners(event);
    }

    /**
     * Get array of event names with registered listeners
     *
     * @example
     * const events = emitter.eventNames();  // ["message", "error"]
     */
    eventNames(): string[] {
        return Array.from(this._events.keys());
    }

    /**
     * Set maximum listeners per event (0 = unlimited)
     * Default is 10. Exceeding triggers a warning (not an error).
     *
     * @example
     * emitter.setMaxListeners(20);
     */
    setMaxListeners(n: number): this {
        this._maxListeners = n;
        return this;
    }

    /**
     * Get maximum listeners setting
     */
    getMaxListeners(): number {
        return this._maxListeners;
    }

    /**
     * Prepend listener (add to beginning of listener array)
     */
    prependListener(event: string, listener: Listener): this {
        let listeners = this._events.get(event);
        if (!listeners) {
            listeners = [];
            this._events.set(event, listeners);
        }

        listeners.unshift({ fn: listener, once: false });
        return this;
    }

    /**
     * Prepend one-time listener
     */
    prependOnceListener(event: string, listener: Listener): this {
        let listeners = this._events.get(event);
        if (!listeners) {
            listeners = [];
            this._events.set(event, listeners);
        }

        listeners.unshift({ fn: listener, once: true });
        return this;
    }
}

/**
 * Default max listeners (can be changed globally)
 */
export const defaultMaxListeners: number = 10;

/**
 * Utility: Create a promise that resolves on first event
 *
 * @example
 * const data = await once(emitter, "data");
 */
export function once(emitter: EventEmitter, event: string): Promise<any[]> {
    return new Promise((resolve, reject) => {
        const onEvent = (...args: any[]) => {
            emitter.off("error", onError);
            resolve(args);
        };

        const onError = (err: Error) => {
            emitter.off(event, onEvent);
            reject(err);
        };

        emitter.once(event, onEvent);
        if (event !== "error") {
            emitter.once("error", onError);
        }
    });
}

/**
 * Get count of listeners for event on any emitter
 */
export function listenerCount(emitter: EventEmitter, event: string): number {
    return emitter.listenerCount(event);
}
