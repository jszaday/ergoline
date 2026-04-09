# Examples Rewritten in EIR Syntax

Five examples from the original `examples/` directory, rewritten to the new language.
These favor idiomatic EIR over line-by-line transliteration. Notes call out semantic or
stylistic choices only when they clarify something non-obvious.

The code blocks are intended to read as first-class EIR examples. Historical deltas are
kept below each example in separate **Migration Notes** sections so the examples
themselves do not have to carry that explanation inline.

---

## `fib.erg` — Fibonacci (actor model, abstract proxies)

The simplest showcase of the actor model. Structurally it stays close to the original,
but the code is written in the style the new language should encourage.

```ergoline
package examples;

import ergoline::_;

@main class main with acceptor {
    val n: i32;

    def self(args: array<string>) {
        n = args.size() > 1
            ? (args[1].parse<i32>() ?? 0)
            : math::max(2 * configuration.threshold, 32);
        new fib@(self@, n);
    }

    @entry override def accept(x: i32): unit {
        println(f"fib($n) = $x");
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
    val threshold: i32 = 16;
}

trait acceptor {
    @entry def accept(x: i32): unit;
}
```

**Migration Notes**
- `int` → `i32`
- `` `fib(${n}) = ${x}` `` → `f"fib($n) = $x"` (bare `$name` for simple identifiers)
- `args[1].toInt()` → `args[1].parse<i32>() ?? 0`

---

## `receiver.erg` — Channel producer/consumer

Short example. `ck::channel` remains a library type, while runtime timing/process queries
live under `hc::`.

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

    @entry def receive() {
        for (var i: i32 = 0; i < 16; i += 1) {
            println(f"received value: ${await ch}");
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

**Migration Notes**
- `channel<int>` → `channel<i32>`
- `@threaded @entry def receive()` → `@entry def receive()`
- `` `received value: ${await ch}` `` → `f"received value: ${await ch}"`

---

## `sdagtest.erg` — SDAG stress test (await any, nested forall)

The most SDAG-complete example. It shows `await any`, nested `forall`, and the full
mailbox-pattern style.

```ergoline
package examples;

import ergoline::_;
import hc;
import blas;

@main class main {
    def self() {
        val n: i64 = hc::numPes() * 4;
        new sdagtest@array1d(n, n, n * 4, self@);
    }

    @entry def done(count: i64) {
        println(f"main> done, count = $count.");
        exit();
    }
}

class sdagtest {
    val neighbors: array<i64>;
    val mainProxy: main@;
    val numIts:    i64;
    val n:         i64;

    @entry def self(=n: i64, =numIts: i64, =mainProxy: main@) {
        val idx = self[@]index();
        neighbors    = new array<i64>(2);
        neighbors[0] = (idx + n - 1) % n;
        neighbors[1] = (idx + 1) % n;
        self[@]run();
        self[@]valid(idx);
    }

    @mailbox def valid(idx: i64);
    @mailbox def invalid();
    @mailbox def receive(it: i64, from: i64);

    @entry def run() {
        val idx = self[@]index();

        await any {
            when valid(_ == idx) =>
                println(f"ch($idx)> received a message from itself!");
            when invalid() =>
                abort("you shouldn't see this!");
        }

        for (var it: i64 = 0; it < numIts; it += 1) {
            for (var i: i64 = 0; i < neighbors.size(); i += 1) {
                self@[neighbors[i]].receive(it, idx);
            }
        }

        forall (it <- 0 to numIts) {
            forall (i <- 0 to neighbors.size()) {
                when receive(_ == it, _ == neighbors[i]) => {
                    println(f"ch$idx> received from ch${neighbors[i]} for iteration $it.");
                }
            }
        }

        self[@]contribute(idx, i64::+, mainProxy.done);
    }
}
```

**Migration Notes**
- `int` → `i64` throughout (sizes, indices, iteration counts)
- `array<int, 1>` → `array<i64>` (1D is implicit)
- `@threaded @entry` → `@entry`
- Nested `@overlap for` → nested `forall`
- idiomaticity: use the shorter `hc::` runtime namespace instead of `runtime::`
- Backtick f-strings → `f"..."`
- type suffixes are omitted unless they disambiguate something important

---

## `cannon.erg` — Cannon's matrix multiplication (compound when, forall, BLAS FFI)

The compound `when` across two mailboxes already reads naturally. The rewrite mostly just
removes old naming debt and uses the modern runtime namespace.

```ergoline
package examples;

import ergoline::_;
import hc;

@system(fromHeader="rand48_replacement.h")
def drand48(): f64;

@system(fromHeader="cmath")
def sqrt(value: f64): f64;

@main class cannon {
    var startTime: f64;
    var endTime:   f64;

    @entry def self(args: array<string>) {
        val alpha:     f64 = 1.0;
        val np:        i64 = hc::numPes();
        val npPerDim:  i64 = sqrt(np as f64) as i64;
        val nbPerDim:  i64 = (np > 1 && npPerDim == 1) ? 2 : npPerDim;
        val blockSize: i64 = (args.size() > 1
            ? (args[1].parse<i64>() ?? 128)
            : 128) / nbPerDim;
        val blockShape = (blockSize, blockSize);

        println(f"matmul starting with a ${npPerDim} x ${npPerDim} mesh on ${np} pes");
        println(f"each block contains ${blockSize} x ${blockSize} values");

        val a = new block@array2d(nbPerDim, nbPerDim, self@, blockShape, nbPerDim, true);
        val b = new block@array2d(nbPerDim, nbPerDim, self@, blockShape, nbPerDim, true);
        val c = new block@array2d(nbPerDim, nbPerDim, self@, blockShape, nbPerDim, false);

        startTime = hc::wallTime();
        a.sendData(c, true);
        b.sendData(c, false);
        c.run(alpha);
    }

    @entry def done() {
        endTime = hc::wallTime();
        println(f"matmul finished in ${(endTime - startTime) * 1000.0} ms");
        exit();
    }
}

class block {
    val shape:     (i64, i64);
    val nBlocks:   i64;
    val data:      array<f64, 2>;
    val mainProxy: cannon@;

    @entry def self(=mainProxy: cannon@, =shape: (i64, i64), =nBlocks: i64, randInit: bool) {
        if (randInit) {
            data = new array<f64, 2>(shape[0], shape[1]);
            for (var i: i64 = 0; i < shape[0]; i += 1) {
                for (var j: i64 = 0; j < shape[1]; j += 1) {
                    data[i, j] = drand48();
                }
            }
        } else {
            data = array<f64, 2>::fill(shape, 0.0);
        }
    }

    @mailbox def inputA(block: i64, arr: array<f64, 2>);
    @mailbox def inputB(block: i64, arr: array<f64, 2>);

    @entry def sendData(dest: block@array2d, sendA: bool) {
        val idx = self[@]index();
        if (sendA) {
            dest[(idx[0] - idx[1] + nBlocks) % nBlocks, idx[1]].inputA(0, data);
        } else {
            dest[idx[0], (idx[1] - idx[0] + nBlocks) % nBlocks].inputB(0, data);
        }
    }

    @entry def run(alpha: f64) {
        val idx = self[@]index();

        forall (block <- 0 to nBlocks) {
            when inputA(_ == block, blockA: array<f64, 2>),
                 inputB(_ == block, blockB: array<f64, 2>) => {

                blas::dgemm(alpha, blockA, blockB, 0.0, data);

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

**Migration Notes**
- `int` → `i64` for all dimensions, block counts, and indices
- `double` → `f64`; `array<double, 2>` → `array<f64, 2>`
- float suffixes are omitted when `f64` is already implied by context
- explicit numeric conversions now use `as`, e.g. `sqrt(np as f64) as i64`
- `@threaded @entry def run(...)` → `@entry def run(...)`
- `@overlap for` → `forall`
- idiomaticity: use `hc::wallTime()` / `hc::numPes()` rather than the longer `runtime::...`
- explicitly import `blas` before using `blas::dgemm`
- `args[1].toInt()` → `args[1].parse<i64>() ?? 128`
- Backtick f-strings → `f"..."`
- The compound `when inputA(...), inputB(...)` is **identical** — it was already right

---

## `jacobi2d.erg` — Jacobi 2D relaxation (full SDAG, dissertation example)

The most significant rewrite. The old global-update choreography is gone, startup is
direct, and the hot loop reads like native EIR instead of translated Charm++.

```ergoline
package examples;

import ergoline::_;
import hc;

object globals {
    val north: i32 = 1;
    val east:  i32 = 2;
    val south: i32 = 3;
    val west:  i32 = 4;

    var maxIters:   i64 = 250;
    var threshold:  f64 = 0.004;
    var mainProxy:  main@;

    var arrDimX:   i64 = 128;
    var arrDimY:   i64 = 128;
    var blockDimX: i64 = 64;
    var blockDimY: i64 = 64;

    var numChareX: i64;
    var numChareY: i64;
}

@main class main {
    var startTime: f64;

    @entry def self(args: array<string>) {
        globals.mainProxy = self@;

        if (args.size() >= 3) {
            globals.arrDimX   = args[1].parse<i64>() ?? 128;
            globals.arrDimY   = globals.arrDimX;
            globals.blockDimX = args[2].parse<i64>() ?? 64;
            globals.blockDimY = globals.blockDimX;
            if (args.size() >= 4) {
                globals.maxIters = args[3].parse<i64>() ?? 250;
            }
        }

        globals.numChareX = globals.arrDimX / globals.blockDimX;
        globals.numChareY = globals.arrDimY / globals.blockDimY;

        println("\nSTENCIL COMPUTATION WITH NO BARRIERS");
        println(f"Running Jacobi on ${hc::numPes()} processors with (${globals.numChareX}, ${globals.numChareY}) chares");
        println(f"Array Dimensions: [${globals.arrDimX}, ${globals.arrDimY}]");
        println(f"Block Dimensions: [${globals.blockDimX}, ${globals.blockDimX}]");
        println(f"Max Iterations: ${globals.maxIters}");
        println(f"Threshold: ${globals.threshold}");

        startTime = hc::wallTime();
        val workers = new jacobi2d@array2d(globals.numChareX, globals.numChareY);
        workers.run();
    }

    @entry def done(numIters: i64) {
        val time = hc::wallTime() - startTime;
        if (numIters >= globals.maxIters) {
            println(f"main> did not converge, finished $numIters iterations in $time s.");
        } else {
            println(f"main> converged in $numIters iterations in $time s.");
        }
        exit();
    }
}

class jacobi2d {
    var numNeighbors: i64 = 0;

    val hasEast:  bool;
    val hasWest:  bool;
    val hasNorth: bool;
    val hasSouth: bool;

    val iStart:  i64 = 1;
    val jStart:  i64 = 1;
    val iFinish: i64;
    val jFinish: i64;

    var grid:     array<f64, 2>;
    var nextGrid: array<f64, 2>;

    @entry def self() {
        val shape = (globals.blockDimX + 2, globals.blockDimY + 2);
        nextGrid  = new array<f64, 2>(shape[0], shape[1]);
        grid      = array<f64, 2>::fill(shape, 0.0);

        val (x, y) = self[@]index();

        def inBounds(num: i64, max: i64): (bool, bool) {
            return (num != 0, num != (max - 1));
        }

        (hasEast,  hasWest)  = inBounds(x, globals.numChareX);
        (hasNorth, hasSouth) = inBounds(y, globals.numChareY);

        numNeighbors = (hasEast as i64) + (hasWest as i64)
                     + (hasNorth as i64) + (hasSouth as i64);

        iStart  = (!hasEast as i64)  + 1;
        jStart  = (!hasNorth as i64) + 1;
        iFinish = globals.blockDimX + 1 - (!hasWest as i64);
        jFinish = globals.blockDimY + 1 - (!hasSouth as i64);

        enforce_boundaries();
    }

    @mailbox def receive_ghost(it: i64, dir: i32, data: array<f64>);
    @mailbox def receive_status(g_converged: bool);

    @entry def run() {
        var converged = false;
        var it: i64   = 0;

        for (; !converged && it < globals.maxIters; it += 1) {
            start_iteration(it);

            forall (_ <- 0 to numNeighbors) {
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

        if (self[@]index() == (0, 0)) {
            globals.mainProxy.done(it);
        }
    }

    private def start_iteration(it: i64) {
        val (x, y) = self[@]index();
        val dimX = globals.blockDimX;
        val dimY = globals.blockDimY;

        if (hasNorth) self@[x, y - 1].receive_ghost(it, globals.south, grid[:, 1].toArray());
        if (hasEast)  self@[x - 1, y].receive_ghost(it, globals.west,  grid[1, :].toArray());
        if (hasSouth) self@[x, y + 1].receive_ghost(it, globals.north, grid[:, dimY].toArray());
        if (hasWest)  self@[x + 1, y].receive_ghost(it, globals.east,  grid[dimX, :].toArray());
    }

    private def enforce_boundaries() {
        val dimX = globals.blockDimX;
        val dimY = globals.blockDimY;
        val default = 1.0;

        def setColumn(j: i64) { grid[:, j] = default; nextGrid[:, j] = default; }
        def setRow(i: i64)    { grid[i, :] = default; nextGrid[i, :] = default; }

        if (!hasNorth) setColumn(1);
        if (!hasEast)  setRow(1);
        if (!hasSouth) setColumn(dimY);
        if (!hasWest)  setRow(dimX);
    }

    private def process_ghost(dir: i32, data: array<f64>) {
        val dimX = globals.blockDimX;
        val dimY = globals.blockDimY;

        if      (dir == globals.north && hasNorth) grid[:, 0]        = data;
        else if (dir == globals.east  && hasEast)  grid[0, :]        = data;
        else if (dir == globals.south && hasSouth) grid[:, dimY + 1] = data;
        else if (dir == globals.west  && hasWest)  grid[dimX + 1, :] = data;
        else abort(f"fatal> unexpected ghost from dir $dir");
    }

    private def check_and_compute(): f64 {
        var maxDiff = 0.0;

        for (var i: i64 = iStart; i < iFinish; i += 1) {
            for (var j: i64 = jStart; j < jFinish; j += 1) {
                nextGrid[i, j] = 0.2 * (
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

**Migration Notes**
- `int` → `i64` for dimensions, indices, iteration counters, and neighbor counts
- `double` → `f64`; `array<double, 2>` → `array<f64, 2>`; `array<double>` → `array<f64>`
- float suffixes are omitted when `f64` is already implied by context
- `@threaded @entry def run()` → `@entry def run()`
- `for (var imsg = 0; ...) { when ... }` → `forall (_ <- 0 to numNeighbors) { when ... }`
- bool→integer casts use ordinary `as`; `!hasSouth as i64` parses as `(!hasSouth) as i64`
- `ck::updateGlobal<globals>(self@initialize)` + separate `initialize()` entry method → **gone**
- idiomaticity: use `hc::wallTime()` / `hc::numPes()` rather than the longer `runtime::...`
- `args[N].toInt()` → `args[N].parse<i64>() ?? default`
- `start_iteration` is a plain local helper, not an async self-entry call
- Backtick f-strings → `f"..."`, bare `$name` for simple identifiers
- unsuffixed integer literals in index position infer `i64`

---

## Summary of universal changes

| Pattern | v1 | EIR |
|---|---|---|
| Integer type | `int` | `i32` (logic) / `i64` (index/size) |
| Float type | `double` | `f64` |
| Float arrays | `array<double, 2>` | `array<f64, 2>` |
| Float literals | `0.0`, `3.14` | unchanged unless a suffix is needed to disambiguate |
| Interpolated strings | `` `hello ${x}` `` | `f"hello $x"` or `f"hello ${x}"` |
| Threaded entry methods | `@threaded @entry def foo()` | `@entry def foo()` |
| Parallel loops | `@overlap for (var i = 0; ...)` | `forall (i <- 0 to n)` |
| Runtime functions | `ck::numPes()`, `ck::wallTime()` | `hc::numPes()`, `hc::wallTime()` |
| String parsing | `.toInt()` | `.parse<i32>() ?? n` |
| Numeric conversions | method-style conversions | explicit `as` casts |
| Distributed globals | `ck::updateGlobal<T>(callback)` | Direct mutation (intra-node) |

---

## Syntax Notes

The examples use `??` for defaulting because it is now specified for `option`/`result`
values:

```ergoline
args[1].parse<i32>() ?? 0
```
