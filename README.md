Pengines Client for F#
========

Pengines is an HTTP service for evaluating Prolog build on the foundation of SWI-Prolog.  A client creates a [Pengine](http://www.swi-prolog.org/pengines/PengineConcept.html), which is a sandboxed Prolog environment, passes any Prolog facts and rules, then queries the facts and rules to obtain a list of solutions.

Modules
-------
This client is designed with F# consumers in mind, and provides the following modules:

* `Pengines.Prolog` - F# discriminated unions for Prolog terms: this includes, atoms, numbers, lists, compound terms, operators, and the dictionary term supported by SWI-Prolog.
* `Pengines.Serialization` - serialize these terms to Prolog, and deserialize them from JSON,
* `Pengines.Pengine` - manage the lifetime of a Pengine instance - create, query, and destroy
* `Pengines.Operators` - helpful operators to make common Prolog operations more succinct from F#

System Requirements
--------

* [SWI-Prolog](http://www.swi-prolog.org/Download.html) - there are distributions available for Linux, macOS, and Windows.
* [Pengines](https://github.com/SWI-Prolog/pengines) - this is a small set of Prolog modules, available in a GitHub repo.
* [.NET Core Runtime](https://www.microsoft.com/net/download/core) - to use this library.  Or you can pull the [single file](src/Pengines.fs) into your F# 4.1 or greater project with [Paket](https://fsprojects.github.io/Paket/).

Usage
-----

1. Create a list of Prolog terms containing your facts and rules
2. Create a Pengine instance with `Pengines.Pengine.createPengine`, passing the list of terms and another term for a query to ask.
3. Iterate through the results.
4. Destroy the Pengine instance, which will delete the facts and rules.  It will timeout and self-destruct after 5 minutes without use if you don't.

Documentation
--------

Please see the [user guide](../../wiki/Using-Pengines.Client) for a walkthrough.

There are also [tests](tests/Tests.fs) that show usage of the complete API.
