# The Great Ergoline Todo List
As we march towards a usable compiler, several things need to be addressed.

## Usability

* Improve User-facing Errors (e.g. Add Candidates to Cannot Resolve Error)

## Code Generation

* Ensure that Generated Classes are Correctly Ordered
* Ensure that Names/Types are Correctly Qualified
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

* Check Overrides Against Base Class
    - Fail When Member Not Marked as Override
    - Ensure Types Match
* Check `@transient` Classes:
    - A class derived from a `@transient` class must be marked as such.
    - A `@transient` type cannot be used as arguments for an entry method.
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
* Add support for templated proxy types.
* Add support for custom array indices (e.g. `foo@array<int>`)
* Use pattern matching for `for` loops and as lval's for assignments:
    - This will enable iterating with and assignment to multiple variables.
    - Use `std::tie` and `std::pair` for the generated code for added niceness.
* Make lambdas and functions pup'able!
* Add a sophisticated containers/iterables layer (`foreach`, `filter`, etc.)

## Things to Ruminate On

* Should we add support for concepts? (i.e. high-level SFINAE)
* Add support for lightweight/`@simple` classes?
    - Generate struct and inline function calls?
    - Enable array-of-structs vs. struct-of-arrays? 
* Should we make blocks expressions a la Scala? And...
    - Get rid of mandatory `;` at EOL?
    - Add an implicit return for last statement of block?
* Should we continue to have `@transient` types?
* Should we get rid of `EirMember`?
* Should we make assignments expressions, or statements?
