Safe, S-expression based, Similar-to-C programming
==================================================

*The following is an outline for the aim. A real implementation has to
be done yet.*

S3C is a programming language using an S-expression based syntax, and
Scheme as macro and test languages. It can be interfaced from(/to?)
Scheme, for testing and incremental porting of code from Scheme. But
the runtime evaluation model is the same as C:

  - no garbage collection, instead making use of stack and explicit
    heap based allocation/deallocation (optionally add reference
    counting later on).

  - explicit references, with 'flat' nested structs

  - no access to types at runtime except [where added explicitely
    manually or] by way of unions

  - where tail call optimization is needed, it has to be done
    explicitely through loops or trampolines

S3C should be compilable/translateable to C code that's well human
readable.

There will be two implementations of the language: a safe, dynamically
typechecked one that allows to develop programs interactively, with
good debugging and introspection possibilities. And one compiled
through C. The latter will [currently] not be safe; the idea is to
instead use extensive automatic (rule based) testing with the safe
implementation to catch issues. Nobody says it's impossible to add
static checking (like with linear types, dependent types, and a proof
language?) later on; if you have insights about how the language
should be shaped to make this easier, please tell. This effort has
limited practical and primarily learning/teaching aims.

The safe implementation checks for:

  - double free and missing free

  - access of freed memory (with no risk of memory reuse shadowing
    such)

  - referencing of stack-allocated data that has gone out of scope

  - bounds checking with any kind of arrays/memory blocks (XXX:
    not thought through yet)

  - types (more restrictive than C, i.e. no implicit casts)

The safe implementation should also allow for redefinition of
procedures without terminating the program, just like Scheme. (That's
also why it even *should* allow (temporarily) type-inconsistent
redefinitions.)  Also the aim is to have debugger support (and
first-class continuations and perhaps the possibility to attach random
data to objects to aid tracing).

Also, the idea is to make extensive use of type wrappers to enforce
invariants (and check them during testing with the safe
implementation); and then have them elided when compiling to C.

[Also, offer parametrized types. Unify variants in the C-compiled code
so that only one per object size is generated?]

The safe implementation should have a mode where (de)allocation errors
are not stopping the program or invoking the debugger, but instead let
the program continue. This should allow to develop programs more
easily (first get it working with regards to types and algorithm, then
get the memory handling right).

Feature additions: see [[Features]]
