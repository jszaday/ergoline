# The Great Ergoline Todo List
As we march towards a usable compiler, several things need to be addressed.

## Usability

* Improve User-facing Errors (e.g. Add Candidates to Cannot Resolve Error)
* Test all the unary and binary ops

## Code Generation

* ~~Ensure that Generated Classes are Correctly Ordered~~
* Ensure that Names/Types are Correctly Qualified (50-70% done)
* Generate Specialized Template Functions/Classes/Proxies.
* Use heap-allocated memory only when necessary/use stack-allocated memory when possible.
    - May consider adding `@simple` annotation to denote when a type should always be stack-allocated.
* Fix multiple inheritance of `std::enable_shared_from_this`:
    - https://stackoverflow.com/questions/16082785/use-of-enable-shared-from-this-with-multiple-inheritance
    - https://stackoverflow.com/questions/657155/how-to-enable-shared-from-this-of-both-parent-and-derived
    - https://stackoverflow.com/questions/15549722/double-inheritance-of-enable-shared-from-this
    - https://stackoverflow.com/questions/4491420/enable-shared-from-this-and-inheritance
    - https://stackoverflow.com/questions/14939190/boost-shared-from-this-and-multiple-inheritance
    - https://www.codeproject.com/Articles/286304/Solution-for-multiple-enable-shared-from-this-in-i

## Correctness Checking

* ~~Check Overloads (compile to templated functions?)~~
* ~~Check Overrides Against Base Class~~
    - ~~Fail When Member Not Marked as Override~~
    - ~~Ensure Types Match~~
* Check `@transient` Classes:
    - A class derived from a `@transient` class must be marked as such.
    - ~~A `@transient` type cannot be used as arguments for an entry method.~~
    - Sub-classes must be marked as `@transient`.
* Regarding staticness:
    - Check that `static` members can only access `static` members.
    - Check that only non-`static` members are accessible via an instance.
    - Check that only `static` members are accessible via a class/type.
* Enforce template constraints.

## Features

* Add storage modifiers for pointer types, e.g. `weak`, `strong`, `unretained`.
* Add template parameter packs (a la C++, but must be non-empty. `unit` is used to denote "nothing")
* Add support for partial/full specialization of functions/classes.
    - Currently considering something like: `class foo<A>` ... `class foo<int!>`
* ~~Add support for templated proxy types.~~
* Add support for custom array indices (e.g. `foo@array<int>`)
* Use pattern matching for `for` loops and as lval's for assignments:
    - This will enable iterating with and assignment to multiple variables.
    - Use `std::tie` and `std::pair` for the generated code for added niceness.
* Make lambdas and functions pup'able! (Preliminarily... done! Need to figure out captures.)
* Add a sophisticated containers/iterables layer (`foreach`, `filter`, etc.)
* Add support for wrapping native iterators.
    - Transform map iterator's `first`/`second` to tuple elements.
* Add support for optional values and variant types.
    - Should we use C++17's `std::optional` or something like: https://github.com/TartanLlama/optional
* Add support for chare placement operations (e.g. `on` for chares, `dmapped` for arrays)

## Things to Ruminate On

* How should we support `null` pointers/uninitialized values? If at all?
* Should we add support for concepts? (i.e. high-level SFINAE)
* Add support for lightweight/`@simple` classes?
    - Generate struct and inline function calls?
    - Enable array-of-structs vs. struct-of-arrays? 
* Should we make blocks  and statements expressions a la Scala? And...
    - Get rid of mandatory `;` at EOL?
    - Add an implicit return for last statement of block?
    - Get rid of the ternary operator.
* Should we continue to have `@transient` types?
* Should we get rid of `EirMember`?
* Should we make assignments expressions, or statements?
