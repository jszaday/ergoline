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
    val _n: i32;

    def self(args: array<string>) {
        _n = args.size() > 1
            ? (args[1].parse<i32>() ?? 0)
            : math::max(2 * configuration.threshold, 32);
        new fib@(self@, _n);
    }

    @entry override def accept(x: i32): unit {
        println(f"fib(${_n}) = $x");
        exit();
    }
}

class fib with acceptor {
    var _count: i32 = 0;
    var _value: i32 = 0;
    val _parent: acceptor@;

    @entry def self(=_parent: acceptor@, n: i32) {
        if (n <= configuration.threshold) {
            _parent.accept(_seqFib(n));
        } else {
            new fib@(self@, n - 1);
            new fib@(self@, n - 2);
        }
    }

    @entry override def accept(x: i32): unit {
        _count += 1;
        _value += x;
        if (_count >= 2) {
            _parent.accept(_value);
        }
    }

    private def _seqFib(n: i32): i32 {
        return (n > 1) ? (_seqFib(n - 1) + _seqFib(n - 2)) : n;
    }
}

object configuration {
    public val threshold: i32 = 16;
}

trait acceptor {
    @entry def accept(x: i32): unit;
}
```

**Migration Notes**
- `int` → `i32`
- `` `fib(${n}) = ${x}` `` → `f"fib($n) = $x"` (bare `$name` for simple identifiers)
- `args[1].toInt()` → `args[1].parse<i32>() ?? 0`
- private state and helper methods are underscored to match the lint policy

---

## `receiver.erg` — Channel producer/consumer

Short example. `ck::channel` remains a library type, while runtime timing/process queries
live under `hc::`.

```ergoline
package examples;

import ergoline::_;
import ck::channel;

@main class receiver {
    val _ch: channel<i32>@;  // channel<int> → channel<i32>

    def self() {
        _ch = new channel<i32>@();
        self@receive();
        new sender@(_ch);
    }

    @entry def receive() {
        for (var i: i32 = 0; i < 16; i += 1) {
            println(f"received value: ${await _ch}");
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
- private members are underscored to match the lint policy
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
    val _neighbors: array<i64>;
    val _mainProxy: main@;
    val _numIts:    i64;
    val _n:         i64;

    @entry def self(=_n: i64, =_numIts: i64, =_mainProxy: main@) {
        val idx = self[@]index();
        _neighbors    = new array<i64>(2);
        _neighbors[0] = (idx + _n - 1) % _n;
        _neighbors[1] = (idx + 1) % _n;
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

        for (var it: i64 = 0; it < _numIts; it += 1) {
            for (var i: i64 = 0; i < _neighbors.size(); i += 1) {
                self@[_neighbors[i]].receive(it, idx);
            }
        }

        forall (it <- 0 to _numIts) {
            forall (i <- 0 to _neighbors.size()) {
                when receive(_ == it, _ == _neighbors[i]) => {
                    println(f"ch$idx> received from ch${_neighbors[i]} for iteration $it.");
                }
            }
        }

        self[@]contribute(idx, i64::+, _mainProxy.done);
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
- private members are underscored to match the lint policy

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
    val _startTime: f64;

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

        _startTime = hc::wallTime();
        a.sendData(c, true);
        b.sendData(c, false);
        c.run(alpha);
    }

    @entry def done() {
        val endTime = hc::wallTime();
        println(f"matmul finished in ${(endTime - _startTime) * 1000.0} ms");
        exit();
    }
}

class block {
    val _shape:     (i64, i64);
    val _nBlocks:   i64;
    val _data:      array<f64, 2>;
    val _mainProxy: cannon@;

    @entry def self(=_mainProxy: cannon@, =_shape: (i64, i64), =_nBlocks: i64, randInit: bool) {
        if (randInit) {
            _data = new array<f64, 2>(_shape[0], _shape[1]);
            for (var i: i64 = 0; i < _shape[0]; i += 1) {
                for (var j: i64 = 0; j < _shape[1]; j += 1) {
                    _data[i, j] = drand48();
                }
            }
        } else {
            _data = array<f64, 2>::fill(_shape, 0.0);
        }
    }

    @mailbox def inputA(block: i64, arr: array<f64, 2>);
    @mailbox def inputB(block: i64, arr: array<f64, 2>);

    @entry def sendData(dest: block@array2d, sendA: bool) {
        val idx = self[@]index();
        if (sendA) {
            dest[(idx[0] - idx[1] + _nBlocks) % _nBlocks, idx[1]].inputA(0, _data);
        } else {
            dest[idx[0], (idx[1] - idx[0] + _nBlocks) % _nBlocks].inputB(0, _data);
        }
    }

    @entry def run(alpha: f64) {
        val idx = self[@]index();

        forall (block <- 0 to _nBlocks) {
            when inputA(_ == block, blockA: array<f64, 2>),
                 inputB(_ == block, blockB: array<f64, 2>) => {

                blas::dgemm(alpha, blockA, blockB, 0.0, _data);

                if ((block + 1) < _nBlocks) {
                    self@[(idx[0] + 1) % _nBlocks, idx[1]].inputA(block + 1, blockA);
                    self@[idx[0], (idx[1] + 1) % _nBlocks].inputB(block + 1, blockB);
                }
            }
        }

        self[@]contribute(_mainProxy.done);
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
- private members are underscored to match the lint policy

---

## `jacobi2d.erg` — Jacobi 2D relaxation (full SDAG, dissertation example)

The most significant rewrite. The old global-update choreography is gone, startup is
direct, and the hot loop reads like native EIR instead of translated Charm++.

```ergoline
package examples;

import ergoline::_;
import hc;

object globals {
    public val north: i32 = 1;
    public val east:  i32 = 2;
    public val south: i32 = 3;
    public val west:  i32 = 4;

    public var maxIters:   i64 = 250;
    public var threshold:  f64 = 0.004;
    public var mainProxy:  main@;

    public var arrDimX:   i64 = 128;
    public var arrDimY:   i64 = 128;
    public var blockDimX: i64 = 64;
    public var blockDimY: i64 = 64;

    public var numChareX: i64;
    public var numChareY: i64;
}

@main class main {
    val _startTime: f64;

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

        _startTime = hc::wallTime();
        val workers = new jacobi2d@array2d(globals.numChareX, globals.numChareY);
        workers.run();
    }

    @entry def done(numIters: i64) {
        val time = hc::wallTime() - _startTime;
        if (numIters >= globals.maxIters) {
            println(f"main> did not converge, finished $numIters iterations in $time s.");
        } else {
            println(f"main> converged in $numIters iterations in $time s.");
        }
        exit();
    }
}

class jacobi2d {
    val _numNeighbors: i64;

    val _hasEast:  bool;
    val _hasWest:  bool;
    val _hasNorth: bool;
    val _hasSouth: bool;

    val _iStart:  i64 = 1;
    val _jStart:  i64 = 1;
    val _iFinish: i64;
    val _jFinish: i64;

    var _grid:     array<f64, 2>;
    var _nextGrid: array<f64, 2>;

    @entry def self() {
        val shape = (globals.blockDimX + 2, globals.blockDimY + 2);
        _nextGrid  = new array<f64, 2>(shape[0], shape[1]);
        _grid      = array<f64, 2>::fill(shape, 0.0);

        val (x, y) = self[@]index();

        def inBounds(num: i64, max: i64): (bool, bool) {
            return (num != 0, num != (max - 1));
        }

        (_hasEast,  _hasWest)  = inBounds(x, globals.numChareX);
        (_hasNorth, _hasSouth) = inBounds(y, globals.numChareY);

        _numNeighbors = (_hasEast as i64) + (_hasWest as i64)
                      + (_hasNorth as i64) + (_hasSouth as i64);

        _iStart  = (!_hasEast as i64)  + 1;
        _jStart  = (!_hasNorth as i64) + 1;
        _iFinish = globals.blockDimX + 1 - (!_hasWest as i64);
        _jFinish = globals.blockDimY + 1 - (!_hasSouth as i64);

        _enforceBoundaries();
    }

    @mailbox def receiveGhost(it: i64, dir: i32, data: array<f64>);
    @mailbox def receiveStatus(gConverged: bool);

    @entry def run() {
        var converged = false;
        var it: i64   = 0;

        for (; !converged && it < globals.maxIters; it += 1) {
            _startIteration(it);

            forall (_ <- 0 to _numNeighbors) {
                when receiveGhost(_ == it, dir, data) => {
                    _processGhost(dir, data);
                }
            }

            val maxDiff = _checkAndCompute();
            converged   = maxDiff <= globals.threshold;

            self[@]contribute(converged, bool::logical_and, self@receiveStatus);

            when receiveStatus(gConverged) => {
                converged = gConverged;
            }
        }

        if (self[@]index() == (0, 0)) {
            globals.mainProxy.done(it);
        }
    }

    private def _startIteration(it: i64) {
        val (x, y) = self[@]index();
        val dimX = globals.blockDimX;
        val dimY = globals.blockDimY;

        if (_hasNorth) self@[x, y - 1].receiveGhost(it, globals.south, _grid[:, 1].copy());
        if (_hasEast)  self@[x - 1, y].receiveGhost(it, globals.west,  _grid[1, :].copy());
        if (_hasSouth) self@[x, y + 1].receiveGhost(it, globals.north, _grid[:, dimY].copy());
        if (_hasWest)  self@[x + 1, y].receiveGhost(it, globals.east,  _grid[dimX, :].copy());
    }

    private def _enforceBoundaries() {
        val dimX = globals.blockDimX;
        val dimY = globals.blockDimY;
        val default = 1.0;

        def setColumn(j: i64) { _grid[:, j] = default; _nextGrid[:, j] = default; }
        def setRow(i: i64)    { _grid[i, :] = default; _nextGrid[i, :] = default; }

        if (!_hasNorth) setColumn(1);
        if (!_hasEast)  setRow(1);
        if (!_hasSouth) setColumn(dimY);
        if (!_hasWest)  setRow(dimX);
    }

    private def _processGhost(dir: i32, data: array<f64>) {
        val dimX = globals.blockDimX;
        val dimY = globals.blockDimY;

        if      (dir == globals.north && _hasNorth) _grid[:, 0]        = data;
        else if (dir == globals.east  && _hasEast)  _grid[0, :]        = data;
        else if (dir == globals.south && _hasSouth) _grid[:, dimY + 1] = data;
        else if (dir == globals.west  && _hasWest)  _grid[dimX + 1, :] = data;
        else abort(f"fatal> unexpected ghost from dir $dir");
    }

    private def _checkAndCompute(): f64 {
        var maxDiff = 0.0;

        for (var i: i64 = _iStart; i < _iFinish; i += 1) {
            for (var j: i64 = _jStart; j < _jFinish; j += 1) {
                _nextGrid[i, j] = 0.2 * (
                    _grid[i, j]     +
                    _grid[i - 1, j] + _grid[i + 1, j] +
                    _grid[i, j - 1] + _grid[i, j + 1]
                );
                val diff = math::abs(_nextGrid[i, j] - _grid[i, j]);
                if (diff > maxDiff) maxDiff = diff;
            }
        }

        (_grid, _nextGrid) = (_nextGrid, _grid);
        return maxDiff;
    }
}
```

**Migration Notes**
- `int` → `i64` for dimensions, indices, iteration counters, and neighbor counts
- `double` → `f64`; `array<double, 2>` → `array<f64, 2>`; `array<double>` → `array<f64>`
- float suffixes are omitted when `f64` is already implied by context
- `@threaded @entry def run()` → `@entry def run()`
- `for (var imsg = 0; ...) { when ... }` → `forall (_ <- 0 to _numNeighbors) { when ... }`
- bool→integer casts use ordinary `as`; `!hasSouth as i64` parses as `(!hasSouth) as i64`
- `ck::updateGlobal<globals>(self@initialize)` + separate `initialize()` entry method → **gone**
- idiomaticity: use `hc::wallTime()` / `hc::numPes()` rather than the longer `runtime::...`
- `args[N].toInt()` → `args[N].parse<i64>() ?? default`
- slice/span materialization uses `.copy()` rather than the older `.toArray()`
- `_startIteration` is a plain local helper, not an async self-entry call
- Backtick f-strings → `f"..."`, bare `$name` for simple identifiers
- unsuffixed integer literals in index position infer `i64`
- private members are underscored to match the lint policy
- fields initialized only in their own `self(...)` prefer `val`; the `globals` object stays
  `var`-heavy because its members are assigned from `main.self(...)`, not from a
  constructor of `globals` itself
- `object`/`globals` are still a less-settled part of the language shape than ordinary
  classes and traits

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
