# Introduction to Ergoline
For full documentation visit [mkdocs.org](https://www.mkdocs.org).

## Types
Ergoline has both native and object types. Native types correspond to standard C-like types, and object types to instantiations of classes. Additionally, users may import C/C++ structs/classes into Ergoline as native types, although there may be limitations on this process. Finally, there are various proxy-types that will be discussed later.

## On Variables and Function Parameters
Variables of object-types are references, pointing to a chunk of memory within either the heap or stack. Function parameters are const by default (implying copy semantics), but may be marked as mutable (i.e. accepting a non-const reference). Functions can return values by copy, reliquinshment of object ownership, or shareable reference. 

# Parallel Constructs

## Proxies
_Proxies_ are the mechanism by which parallel objects or chares are accessed. They can be thought of as a wrapper around the parallel object, providing _entry methods_ to access it. Thus, functions marked with the "entry" keyword are available via proxies. Entry methods may not have mutable parameters, and their parameters must be of pup'able types. Entry methods support returning values with copy or ownership reliquishment semantics. Values returned with copy semantics are automatically wrapped as futures on the receiving end, and only future values can be returned via ownership reliquishment. Calls to entry methods are implemented as remote-method invocations; the underlying messaging mechanisms are hidden from the user (parallel object location, parameter marshalling, etc.). Proxies can be instantiated for classes implementing the `ck::chare` abstract class, and are marked with an atpersand (`@`) suffix.

    class test extends ck::chare {
        entry do_something(var self: test): int {
            return 42;
        }
    }
    ...
    test@ testProxy = new test@(...);
    int ret = await test@::do_something(testProxy);

## Proxy Collections
Charm++, and thereby Erg, has support for various types of collections of chares. The most generic of these are chare arrays, which have no inherent sizing or placement restrictions. Classes implementing the `ck::array::element` abstract class can be instantiated as chare-arrays using the `@array` suffix:

    class test extends ck::array::element { ... }
    ...
    test@array arrProxy = new test@array(...);

Likewise for `@nodegroup` and `@group`.
