# Introduction to Ergoline
Ergoline requires features from the master branch of Charm++, accessible at `CHARM_HOME`. Ensure that `ERG_HOME` is set to the base directory of this repository. To get started with it, you will need either a Java installation and a pre-built `ergc.jar` file (available as a CI artifact) or an SBT toolchain (we test with SBT1.4.2 and JDK8+). 

Running `sbt "run <.erg file>"` will compile files via SBT. As for using a Jarfile, we recommend aliasing `ergc` via `alias ergc="java -jar $ERG_HOME/ergc.jar"`. This enables straightforward commands such as `ergc <.erg file>`. One can manually build a Jarfile using `sbt assembly`. Note, the compiler will search `ERG_CLASSPATH` for modules.

## Types
Ergoline has both native and object types. Tuples and lambda types are native, first-class citizens. Native types correspond to standard C/C++ types, and object types to instantiations of classes. Additionally, users may generate wrappers for C/C++ structs/classes into Ergoline as native types via the `@system` annotation (see `libs/ergoline` for examples). Finally, there are various proxy-types that will be discussed later.

### On Variables and Function Parameters
All variables of object-types are smart pointers to heap-allocated memory; users need not be concerned with memory management. Argument passed between non-entry methods are passed by modifiable reference; Ergoline has no notion of "const". Arguments of entry methods always have copy semantics. Since threads can only communicate with other threads via entry methods or atomic variables, data-races are not possible.

### On Pupp'ing
Ergoline automatically generates `pup` methods for its object-types, pupp'ing all contained fields (and calling the pup'pers of parent classes). This behavior may not always be desired, so users are encouraged to use the `@transient` annotation to exclude fields from auto-pup'ing or, with discretion, manually specify their own `pup` functions.

# Parallel Constructs
The examples below need to be updated, so please refer to `examples` for up-to-date _and_ unit-tested examples :)

## Proxies
_Proxies_ are the mechanism by which parallel objects or chares are accessed. They can be thought of as a wrapper around the parallel object, providing _entry methods_ to access it. Thus, functions marked with the "@entry" annotation are available via proxies. Entry methods may not have mutable parameters, and their parameters must be of pup'able types. Entry methods support returning values with copy or ownership reliquishment semantics. Values returned with copy semantics are automatically wrapped as futures on the receiving end, and only future values can be returned via ownership reliquishment. Calls to entry methods are implemented as remote-method invocations; the underlying messaging mechanisms are hidden from the user (parallel object location, parameter marshalling, etc.). Proxies can be instantiated for classes with entry constructors, and are marked with an atpersand (`@`) suffix.

    class test {
        @entry def test(selfProxy: test@) { ... }
        @async @entry def do_something(): int {
            return 42;
        }
    }
    ...
    val testProxy = new test@(...);
    val ret = await testProxy.do_something();

## Proxy Collections
Charm++, and thereby Erg, has support for various types of collections of chares. The most generic of these are chare arrays, which have no inherent sizing or placement restrictions. Classes with an appropriate can be instantiated as chare-arrays using the `@array1d` (`@array2d`, etc.) proxy suffix:

    class test {
        // note a chare array element accepts an element proxy, denoted by [@] (an element) vs @ (the collective)
        @entry def test(selfProxy: test[@]array1d) { ... }  
    ...
    val arrProxy: test@array1d = new test@array1d(...);

Likewise for `@nodegroup` and `@group`.
