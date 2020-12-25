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
* Add support for concepts? (i.e. high-level SFINAE)
