// Background Worker for LSP Revalidation
//
// Implements stale-while-revalidate pattern:
// 1. Return stale cached data immediately for responsiveness
// 2. Queue background job to refresh the cache
// 3. Next request gets fresh data
//
// Current implementation: Synchronous job processing
// Future: Can be extended with std.Thread for true async

const std = @import("std");

/// Job priority levels (higher = more important)
pub const Priority = enum(u8) {
    /// Currently visible file - highest priority
    visible = 3,
    /// Imported by visible file
    imported = 2,
    /// Other open files
    open = 1,
    /// Background pre-warming
    background = 0,
};

/// Job types for background processing
pub const JobType = enum {
    /// Revalidate macro expansion
    macro_expand,
    /// Revalidate type inference
    type_check,
    /// Pre-warm file on open
    prewarm,
    /// Refresh completion cache
    completion_refresh,
};

/// A background job to be processed
pub const Job = struct {
    job_type: JobType,
    priority: Priority,
    /// Cache key to revalidate
    cache_key: u64,
    /// File being processed
    file_id: []const u8,
    /// Cancellation version at queue time (skip if stale)
    queued_at_version: u64,
};

/// Background worker manages a priority queue of revalidation jobs
pub const BackgroundWorker = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    /// Priority queue of pending jobs
    jobs: std.ArrayList(Job),
    /// Current cancellation version (from Trans-Am)
    current_version: *std.atomic.Value(u64),
    /// Statistics
    stats: Stats,

    pub const Stats = struct {
        jobs_queued: u64 = 0,
        jobs_processed: u64 = 0,
        jobs_cancelled: u64 = 0,
        jobs_skipped_stale: u64 = 0,
    };

    pub fn init(allocator: std.mem.Allocator, cancellation_version: *std.atomic.Value(u64)) Self {
        return .{
            .allocator = allocator,
            .jobs = std.ArrayList(Job).init(allocator),
            .current_version = cancellation_version,
            .stats = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        // Free any owned file_id strings
        for (self.jobs.items) |job| {
            self.allocator.free(job.file_id);
        }
        self.jobs.deinit();
    }

    /// Queue a job for background processing
    pub fn queue(self: *Self, job_type: JobType, priority: Priority, cache_key: u64, file_id: []const u8) !void {
        const owned_file_id = try self.allocator.dupe(u8, file_id);
        errdefer self.allocator.free(owned_file_id);

        try self.jobs.append(.{
            .job_type = job_type,
            .priority = priority,
            .cache_key = cache_key,
            .file_id = owned_file_id,
            .queued_at_version = self.current_version.load(.seq_cst),
        });
        self.stats.jobs_queued += 1;

        // Keep sorted by priority (highest first)
        std.mem.sort(Job, self.jobs.items, {}, struct {
            fn lessThan(_: void, a: Job, b: Job) bool {
                return @intFromEnum(a.priority) > @intFromEnum(b.priority);
            }
        }.lessThan);
    }

    /// Get next job to process (highest priority, not stale)
    pub fn popNext(self: *Self) ?Job {
        const current = self.current_version.load(.seq_cst);

        while (self.jobs.items.len > 0) {
            const job = self.jobs.orderedRemove(0);

            // Skip if job was queued before current cancellation
            if (job.queued_at_version != current) {
                self.allocator.free(job.file_id);
                self.stats.jobs_skipped_stale += 1;
                continue;
            }

            return job;
        }

        return null;
    }

    /// Process all pending jobs synchronously
    /// In future: this could run on a separate thread
    pub fn processAll(self: *Self, processor: anytype) void {
        while (self.popNext()) |job| {
            defer self.allocator.free(job.file_id);

            // Check cancellation before processing
            const current = self.current_version.load(.seq_cst);
            if (job.queued_at_version != current) {
                self.stats.jobs_cancelled += 1;
                continue;
            }

            // Process the job
            processor.process(job) catch |err| {
                std.log.warn("Background job failed: {}", .{err});
            };

            self.stats.jobs_processed += 1;
        }
    }

    /// Cancel all pending jobs (called on file change)
    pub fn cancelAll(self: *Self) void {
        for (self.jobs.items) |job| {
            self.allocator.free(job.file_id);
        }
        self.jobs.clearRetainingCapacity();
        self.stats.jobs_cancelled += self.jobs.items.len;
    }

    /// Get number of pending jobs
    pub fn pendingCount(self: *Self) usize {
        return self.jobs.items.len;
    }

    /// Get statistics
    pub fn getStats(self: *Self) Stats {
        return self.stats;
    }
};

// ===== TESTS =====

test "BackgroundWorker: queue and pop" {
    var version = std.atomic.Value(u64).init(1);
    var worker = BackgroundWorker.init(std.testing.allocator, &version);
    defer worker.deinit();

    try worker.queue(.macro_expand, .visible, 123, "file1.ms");
    try worker.queue(.type_check, .background, 456, "file2.ms");

    try std.testing.expectEqual(@as(usize, 2), worker.pendingCount());

    // Should get visible priority first
    const job1 = worker.popNext().?;
    defer std.testing.allocator.free(job1.file_id);
    try std.testing.expectEqual(Priority.visible, job1.priority);

    const job2 = worker.popNext().?;
    defer std.testing.allocator.free(job2.file_id);
    try std.testing.expectEqual(Priority.background, job2.priority);

    try std.testing.expectEqual(@as(?Job, null), worker.popNext());
}

test "BackgroundWorker: stale jobs skipped" {
    var version = std.atomic.Value(u64).init(1);
    var worker = BackgroundWorker.init(std.testing.allocator, &version);
    defer worker.deinit();

    try worker.queue(.macro_expand, .visible, 123, "file1.ms");

    // Increment version (simulates file change)
    _ = version.fetchAdd(1, .seq_cst);

    // Job should be skipped as stale
    try std.testing.expectEqual(@as(?Job, null), worker.popNext());
    try std.testing.expectEqual(@as(u64, 1), worker.stats.jobs_skipped_stale);
}

test "BackgroundWorker: priority ordering" {
    var version = std.atomic.Value(u64).init(1);
    var worker = BackgroundWorker.init(std.testing.allocator, &version);
    defer worker.deinit();

    // Queue in reverse priority order
    try worker.queue(.prewarm, .background, 1, "bg.ms");
    try worker.queue(.macro_expand, .visible, 2, "visible.ms");
    try worker.queue(.type_check, .imported, 3, "imported.ms");

    // Should come out in priority order
    const job1 = worker.popNext().?;
    defer std.testing.allocator.free(job1.file_id);
    try std.testing.expectEqual(Priority.visible, job1.priority);

    const job2 = worker.popNext().?;
    defer std.testing.allocator.free(job2.file_id);
    try std.testing.expectEqual(Priority.imported, job2.priority);

    const job3 = worker.popNext().?;
    defer std.testing.allocator.free(job3.file_id);
    try std.testing.expectEqual(Priority.background, job3.priority);
}
