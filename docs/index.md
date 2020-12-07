# Introduction to Ergoline
Ergoline requires a build of Charm++ (v6.9+) accessible via `CHARM_HOME`. Ensure that `ERG_HOME` is set to the base directory of this repository. To get started with it, you will need either an installation of Java and a release `.JAR` file or SBT toolchain (we test with SBT1.4.2 and JDK8+). 

Running `sbt "run <.erg file>"` will compile files via SBT. As for Java, we recommend aliasing `ergc` with your `.JAR` file (e.g. `alias ergc="java -jar $ERG_HOME/bin/erg.jar"`); this enables straightforward command-line issuances such as `ergc <.erg file>`. Note, the compiler will search `ERG_CLASSPATH` for modules.

## Types
Ergoline has both native and object types. Tuples and lambda types are native, first-class citizens. Native types correspond to standard C/C++ types, and object types to instantiations of classes. Additionally, users may generate wrappers for C/C++ structs/classes into Ergoline as native types via the `@system` annotation (see `libs/ergoline` for examples). Finally, there are various proxy-types that will be discussed later.

### On Variables and Function Parameters
All variables of object-types are smart pointers to heap-allocated memory; users need not be concerned with memory management. Argument passed between non-entry methods are passed by modifiable reference; Ergoline has no notion of "const". Arguments of entry methods always have copy semantics. Since threads can only communicate with other threads via entry methods or atomic variables, data-races are not possible.

### On Pupp'ing
Ergoline automatically generates `pup` methods for its object-types, pupp'ing all contained fields (and calling the pup'pers of parent classes). This behavior may not always be desired, so users are encouraged to use the `@transient` annotation to exclude fields from auto-pup'ing or, with discretion, manually specify their own `pup` functions.

# Parallel Constructs
The examples below need to be updated, so please refer to `examples` for up-to-date _and_ unit-tested examples :)

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
