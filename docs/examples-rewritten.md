# Examples Rewritten in EIR Syntax

Five examples from the original `examples/` directory, rewritten to the new language.
Annotations mark every meaningful change from the original.

---

## `fib.erg` — Fibonacci (actor model, abstract proxies)

The simplest showcase of the actor model. Unchanged structurally — just type names and
string interpolation.

```ergoline
package examples;

import ergoline::_;

@main class main with acceptor {
    val n: i32;  // int → i32

    def self(args: array<string>) {
        n = args.size() > 1
            ? args[1].parse<i32>().getOrElse(0)  // .toInt() → .parse<i32>().getOrElse
            : math::max(2 * configuration.threshold, 32);
        new fib@(self@, n);
    }

    @entry override def accept(x: i32): unit {
        println(f"fib($n) = $x");  // backtick f-string → f"..."  $n bare, ${x} not needed
        exit();
    }
}

class fib with acceptor {
    var count: i32 = 0;
    var value: i32 = 0;
    val parent: acceptor@;

    @entry def self(=parent: acceptor@, n: i32) {
        if (n <= configuration.threshold) {
            parent.accept(seq_fib(n));
        } else {
            new fib@(self@, n - 1);
            new fib@(self@, n - 2);
        }
    }

    @entry override def accept(x: i32): unit {
        count += 1;
        value += x;
        if (count >= 2) {
            parent.accept(value);
        }
    }

    private def seq_fib(n: i32): i32 {
        return (n > 1) ? (seq_fib(n - 1) + seq_fib(n - 2)) : n;
    }
}

object configuration {
    val threshold: i32 = 16;  // int → i32
}

trait acceptor {
    @entry def accept(x: i32): unit;  // int → i32
}
```

**Changes from v1:**
- `int` → `i32`
- `` `fib(${n}) = ${x}` `` → `f"fib($n) = $x"` (bare `$name` for simple identifiers)
- `args[1].toInt()` → `args[1].parse<i32>().getOrElse(0)` — explicit error handling

---

## `receiver.erg` — Channel producer/consumer

Short example; the main change is that `@threaded @entry` collapses to `@entry` — the
compiler detects the `await` suspension point and generates the coroutine automatically.

```ergoline
package examples;

import ergoline::_;
import ck::channel;

@main class receiver {
    val ch: channel<i32>@;  // channel<int> → channel<i32>

    def self() {
        ch = new channel<i32>@();
        self@receive();
        new sender@(ch);
    }

    // @threaded @entry → @entry (suspension detected from `await ch`)
    @entry def receive() {
        for (var i: i32 = 0; i < 16; i += 1) {
            println(f"received value: ${await ch}");  // f-string
        }
        exit();
    }
}

class sender {
    @entry def self(ch: channel<i32>@) {
        for (var i: i32 = 0; i < 16; i += 1) {
            ch.send(i);
        }
    }
}
```

**Changes from v1:**
- `channel<int>` → `channel<i32>`
- `@threaded @entry def receive()` → `@entry def receive()`
- `` `received value: ${await ch}` `` → `f"received value: ${await ch}"`

---

## `sdagtest.erg` — SDAG stress test (await any, nested forall)

The most SDAG-complete example. Shows `await any`, nested `forall` replacing double
`@overlap for`, and the full when-clause pattern matching.

```ergoline
package examples;

import ergoline::_;
import runtime;  // ck:: runtime primitives → runtime::

@main class main {
    def self() {
        val n: u64 = runtime::numPes() * 4;  // ck::numPes() → runtime::numPes(); u64 index
        new sdagtest@array1d(n, n, n * 4, self@);
    }

    @entry def done(count: u64) {  // int → u64 (a sum of indices)
        println(f"main> done, count = $count.");
        exit();
    }
}

class sdagtest {
    val neighbors: array<u64>;     // array<int, 1> → array<u64>
    val mainProxy: main@;
    val numIts:    u64;            // int → u64
    val n:         u64;

    @entry def self(=n: u64, =numIts: u64, =mainProxy: main@) {
        val idx: u64 = self[@]index();
        neighbors    = new array<u64>(2);
        neighbors[0] = (idx + n - 1) % n;
        neighbors[1] = (idx + 1) % n;
        self[@]run();
        self[@]valid(idx);
    }

    @mailbox def valid(idx: u64);
    @mailbox def invalid();
    @mailbox def receive(it: u64, from: u64);

    // @threaded @entry → @entry
    @entry def run() {
        val idx: u64 = self[@]index();

        // await any is unchanged
        await any {
            when valid(_ == idx) =>
                println(f"ch($idx)> received a message from itself!");
            when invalid() =>
                abort("you shouldn't see this!");
        }

        for (var it: u64 = 0; it < numIts; it += 1) {
            for (var i: u64 = 0; i < neighbors.size(); i += 1) {
                self@[neighbors[i]].receive(it, idx);
            }
        }

        // double @overlap for → nested forall
        // outer: one coroutine per iteration; inner: one per neighbor
        forall (it <- 0u64 to numIts) {
            forall (i <- 0u64 to neighbors.size()) {
                when receive(_ == it, _ == neighbors[i]) => {
                    println(f"ch$idx> received from ch${neighbors[i]} for iteration $it.");
                }
            }
        }

        self[@]contribute(idx, u64::+, mainProxy.done);
    }
}
```

**Changes from v1:**
- `int` → `u64` throughout (sizes, indices, iteration counts)
- `array<int, 1>` → `array<u64>` (1D is implicit)
- `@threaded @entry` → `@entry`
- Nested `@overlap for` → nested `forall`
- `ck::numPes()` → `runtime::numPes()`
- Backtick f-strings → `f"..."`
- `0` literals in index position infer `u64`; where ambiguous, `0u64` used explicitly

---

## `cannon.erg` — Cannon's matrix multiplication (compound when, forall, BLAS FFI)

The compound `when` across two mailboxes is unchanged — it already reads naturally. The
main work is `@overlap for` → `forall` and type cleanup.

```ergoline
package examples;

import ergoline::_;
import runtime;

@system(fromHeader="rand48_replacement.h")
def drand48(): f64;  // double → f64

@system(fromHeader="cmath")
def sqrt(value: f64): f64;  // double → f64

@main class cannon {
    var startTime: f64;  // double → f64
    var endTime:   f64;

    @entry def self(args: array<string>) {
        val alpha:     f64 = 1.0f64;
        val np:        u64 = runtime::numPes();
        val npPerDim:  u64 = sqrt(np.toF64()).toU64();  // toDouble()→toF64(), toInt()→toU64()
        val nbPerDim:  u64 = (np > 1 && npPerDim == 1) ? 2 : npPerDim;
        val blockSize: u64 = (args.size() > 1
            ? args[1].parse<u64>().getOrElse(128)
            : 128) / nbPerDim;
        val blockShape = (blockSize, blockSize);

        println(f"matmul starting with a ${npPerDim} x ${npPerDim} mesh on ${np} pes");
        println(f"each block contains ${blockSize} x ${blockSize} values");

        val a = new block@array2d(nbPerDim, nbPerDim, self@, blockShape, nbPerDim, true);
        val b = new block@array2d(nbPerDim, nbPerDim, self@, blockShape, nbPerDim, true);
        val c = new block@array2d(nbPerDim, nbPerDim, self@, blockShape, nbPerDim, false);

        startTime = runtime::wallTime();  // ck::wallTime() → runtime::wallTime()
        a.sendData(c, true);
        b.sendData(c, false);
        c.run(alpha);
    }

    @entry def done() {
        endTime = runtime::wallTime();
        println(f"matmul finished in ${(endTime - startTime) * 1000.0f64} ms");
        exit();
    }
}

class block {
    val shape:     (u64, u64);       // (int, int) → (u64, u64)
    val nBlocks:   u64;
    val data:      array<f64, 2>;    // array<double, 2> → array<f64, 2>
    val mainProxy: cannon@;

    @entry def self(=mainProxy: cannon@, =shape: (u64, u64), =nBlocks: u64, randInit: bool) {
        if (randInit) {
            data = new array<f64, 2>(shape[0], shape[1]);
            for (var i: u64 = 0; i < shape[0]; i += 1) {
                for (var j: u64 = 0; j < shape[1]; j += 1) {
                    data[i, j] = drand48();
                }
            }
        } else {
            data = array<f64, 2>::fill(shape, 0.0f64);
        }
    }

    @mailbox def inputA(block: u64, arr: array<f64, 2>);
    @mailbox def inputB(block: u64, arr: array<f64, 2>);

    @entry def sendData(dest: block@array2d, sendA: bool) {
        val idx = self[@]index();
        if (sendA) {
            dest[(idx[0] - idx[1] + nBlocks) % nBlocks, idx[1]].inputA(0, data);
        } else {
            dest[idx[0], (idx[1] - idx[0] + nBlocks) % nBlocks].inputB(0, data);
        }
    }

    // @threaded @entry → @entry
    @entry def run(alpha: f64) {
        val idx = self[@]index();

        // @overlap for → forall; compound when unchanged
        forall (block <- 0u64 to nBlocks) {
            when inputA(_ == block, blockA: array<f64, 2>),
                 inputB(_ == block, blockB: array<f64, 2>) => {

                blas::dgemm(alpha, blockA, blockB, 0.0f64, data);

                if ((block + 1) < nBlocks) {
                    self@[(idx[0] + 1) % nBlocks, idx[1]].inputA(block + 1, blockA);
                    self@[idx[0], (idx[1] + 1) % nBlocks].inputB(block + 1, blockB);
                }
            }
        }

        self[@]contribute(mainProxy.done);
    }
}
```

**Changes from v1:**
- `int` → `u64` for all dimensions, block counts, and indices
- `double` → `f64`; `array<double, 2>` → `array<f64, 2>`
- `0.0` → `0.0f64`, `1.0` → `1.0f64`
- `.toDouble()` → `.toF64()`, `.toInt()` → `.toU64()`
- `@threaded @entry def run(...)` → `@entry def run(...)`
- `@overlap for` → `forall`
- `ck::wallTime()` / `ck::numPes()` → `runtime::wallTime()` / `runtime::numPes()`
- `args[1].toInt()` → `args[1].parse<u64>().getOrElse(128)`
- Backtick f-strings → `f"..."`
- The compound `when inputA(...), inputB(...)` is **identical** — it was already right

---

## `jacobi2d.erg` — Jacobi 2D relaxation (full SDAG, dissertation example)

The most significant rewrite. Beyond type names: `ck::updateGlobal` is gone entirely
(intra-node actors share memory, no broadcast needed), the two-step
constructor→initialize becomes one, and `@overlap for` becomes `forall`.

```ergoline
package examples;

import ergoline::_;
import runtime;

// Singleton with direct field mutation — no ck::updateGlobal needed intra-node
object globals {
    val north: i32 = 1;
    val east:  i32 = 2;
    val south: i32 = 3;
    val west:  i32 = 4;

    var maxIters:   u64 = 250;
    var threshold:  f64 = 0.004f64;
    var mainProxy:  main@;

    var arrDimX:   u64 = 128;
    var arrDimY:   u64 = 128;
    var blockDimX: u64 = 64;
    var blockDimY: u64 = 64;

    var numChareX: u64;
    var numChareY: u64;
}

@main class main {
    var startTime: f64;

    // ck::updateGlobal removed — just set globals and proceed directly
    @entry def self(args: array<string>) {
        globals.mainProxy = self@;

        if (args.size() >= 3) {
            globals.arrDimX   = args[1].parse<u64>().getOrElse(128);
            globals.arrDimY   = globals.arrDimX;
            globals.blockDimX = args[2].parse<u64>().getOrElse(64);
            globals.blockDimY = globals.blockDimX;
            if (args.size() >= 4) {
                globals.maxIters = args[3].parse<u64>().getOrElse(250);
            }
        }

        globals.numChareX = globals.arrDimX / globals.blockDimX;
        globals.numChareY = globals.arrDimY / globals.blockDimY;

        println("\nSTENCIL COMPUTATION WITH NO BARRIERS");
        println(f"Running Jacobi on ${runtime::numPes()} processors with (${globals.numChareX}, ${globals.numChareY}) chares");
        println(f"Array Dimensions: [${globals.arrDimX}, ${globals.arrDimY}]");
        println(f"Block Dimensions: [${globals.blockDimX}, ${globals.blockDimX}]");
        println(f"Max Iterations: ${globals.maxIters}");
        println(f"Threshold: ${globals.threshold}");

        startTime = runtime::wallTime();
        val workers = new jacobi2d@array2d(globals.numChareX, globals.numChareY);
        workers.run();  // no separate initialize() step needed
    }

    @entry def done(numIters: u64) {
        val time = runtime::wallTime() - startTime;
        if (numIters >= globals.maxIters) {
            println(f"main> did not converge, finished $numIters iterations in $time s.");
        } else {
            println(f"main> converged in $numIters iterations in $time s.");
        }
        exit();
    }
}

class jacobi2d {
    var numNeighbors: i32 = 0;  // a count of 4 directions — i32 is fine

    val hasEast:  bool;
    val hasWest:  bool;
    val hasNorth: bool;
    val hasSouth: bool;

    val iStart:  u64 = 1;
    val jStart:  u64 = 1;
    val iFinish: u64;
    val jFinish: u64;

    var grid:     array<f64, 2>;  // array<double, 2> → array<f64, 2>
    var nextGrid: array<f64, 2>;

    @entry def self() {
        val shape = (globals.blockDimX + 2, globals.blockDimY + 2);
        nextGrid  = new array<f64, 2>(shape[0], shape[1]);
        grid      = array<f64, 2>::fill(shape, 0.0f64);

        val (x, y) = self[@]index();

        def inBounds(num: u64, max: u64): (bool, bool) {
            return (num != 0, num != (max - 1));
        }

        (hasEast,  hasWest)  = inBounds(x, globals.numChareX);
        (hasNorth, hasSouth) = inBounds(y, globals.numChareY);

        numNeighbors = hasEast.toI32() + hasWest.toI32()
                     + hasNorth.toI32() + hasSouth.toI32();

        iStart  = (!hasEast).toU64()  + 1;
        jStart  = (!hasNorth).toU64() + 1;
        iFinish = globals.blockDimX + 1 - (!hasWest).toU64();
        jFinish = globals.blockDimY + 1 - (!hasSouth).toU64();

        enforce_boundaries();
    }

    @mailbox def receive_ghost(it: u64, dir: i32, data: array<f64>);
    @mailbox def receive_status(g_converged: bool);

    // @threaded @entry → @entry
    @entry def run() {
        var converged = false;
        var it: u64   = 0;

        for (; !converged && it < globals.maxIters; it += 1) {
            start_iteration(it);

            // inner for-when loop → forall
            forall (_ <- 0u64 to numNeighbors.toU64()) {
                when receive_ghost(_ == it, dir, data) => {
                    process_ghost(dir, data);
                }
            }

            val maxDiff = check_and_compute();
            converged   = maxDiff <= globals.threshold;

            self[@]contribute(converged, bool::logical_and, self@receive_status);

            when receive_status(g_converged) => {
                converged = g_converged;
            }
        }

        if (self[@]index() == (0u64, 0u64)) {
            globals.mainProxy.done(it);
        }
    }

    @entry def start_iteration(it: u64) {
        val (x, y)       = self[@]index();
        val (dimX, dimY) = (globals.blockDimX, globals.blockDimY);

        if (hasNorth) self@[x, y - 1].receive_ghost(it, globals.south, grid[:, 1].toArray());
        if (hasEast)  self@[x - 1, y].receive_ghost(it, globals.west,  grid[1, :].toArray());
        if (hasSouth) self@[x, y + 1].receive_ghost(it, globals.north, grid[:, dimY].toArray());
        if (hasWest)  self@[x + 1, y].receive_ghost(it, globals.east,  grid[dimX, :].toArray());
    }

    private def enforce_boundaries() {
        val (dimX, dimY) = (globals.blockDimX, globals.blockDimY);
        val default      = 1.0f64;

        def setColumn(j: u64) { grid[:, j] = default; nextGrid[:, j] = default; }
        def setRow(i: u64)    { grid[i, :] = default; nextGrid[i, :] = default; }

        if (!hasNorth) setColumn(1);
        if (!hasEast)  setRow(1);
        if (!hasSouth) setColumn(dimY);
        if (!hasWest)  setRow(dimX);
    }

    private def process_ghost(dir: i32, data: array<f64>) {
        val (dimX, dimY) = (globals.blockDimX, globals.blockDimY);

        if      (dir == globals.north && hasNorth) grid[:, 0]        = data;
        else if (dir == globals.east  && hasEast)  grid[0, :]        = data;
        else if (dir == globals.south && hasSouth) grid[:, dimY + 1] = data;
        else if (dir == globals.west  && hasWest)  grid[dimX + 1, :] = data;
        else abort(f"fatal> unexpected ghost from dir $dir");
    }

    private def check_and_compute(): f64 {
        var maxDiff = 0.0f64;

        for (var i: u64 = iStart; i < iFinish; i += 1) {
            for (var j: u64 = jStart; j < jFinish; j += 1) {
                nextGrid[i, j] = 0.2f64 * (
                    grid[i, j]     +
                    grid[i - 1, j] + grid[i + 1, j] +
                    grid[i, j - 1] + grid[i, j + 1]
                );
                val diff = math::abs(nextGrid[i, j] - grid[i, j]);
                if (diff > maxDiff) maxDiff = diff;
            }
        }

        (grid, nextGrid) = (nextGrid, grid);
        return maxDiff;
    }
}
```

**Changes from v1:**
- `int` → `i32` for cardinal directions (small counts); `u64` for all dimensions, indices, and iteration counters
- `double` → `f64`; `array<double, 2>` → `array<f64, 2>`; `array<double>` → `array<f64>`
- `0.0` / `1.0` / `0.2` → `0.0f64` / `1.0f64` / `0.2f64` (explicit float width)
- `@threaded @entry def run()` → `@entry def run()`
- `for (var imsg = 0; ...) { when ... }` → `forall (_ <- 0 to numNeighbors.toU64()) { when ... }`
- `ck::updateGlobal<globals>(self@initialize)` + separate `initialize()` entry method → **gone**. The two-step init was required by Charm++'s distributed broadcast. Intra-node actors share memory; globals are set directly in `def self()` and `workers.run()` is called immediately.
- `ck::wallTime()` / `ck::numPes()` → `runtime::wallTime()` / `runtime::numPes()`
- `args[N].toInt()` → `args[N].parse<u64>().getOrElse(default)`
- Backtick f-strings → `f"..."`, bare `$name` for simple identifiers
- `(0, 0)` index comparison → `(0u64, 0u64)` (explicit to avoid `i32`/`u64` mismatch)

---

## Summary of universal changes

| Pattern | v1 | EIR |
|---|---|---|
| Integer type | `int` | `i32` (logic) / `u64` (index/size) |
| Float type | `double` | `f64` |
| Float arrays | `array<double, 2>` | `array<f64, 2>` |
| Float literals | `0.0`, `3.14` | `0.0f64`, `3.14f64` |
| Interpolated strings | `` `hello ${x}` `` | `f"hello $x"` or `f"hello ${x}"` |
| Threaded entry methods | `@threaded @entry def foo()` | `@entry def foo()` |
| Parallel loops | `@overlap for (var i = 0; ...)` | `forall (i <- 0 to n)` |
| Runtime functions | `ck::numPes()`, `ck::wallTime()` | `runtime::numPes()`, `runtime::wallTime()` |
| String parsing | `.toInt()` | `.parse<i32>().getOrElse(n)` |
| Numeric conversions | `.toDouble()`, `.toInt()` | `.toF64()`, `.toI32()`, `.toU64()` |
| Distributed globals | `ck::updateGlobal<T>(callback)` | Direct mutation (intra-node) |
