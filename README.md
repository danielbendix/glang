<div align="center">

# G

A low-level programming language inspired by C & Swift.

</div>

## Examples
*In order to view code with syntax highlighting, [see the Github Pages version of this document.](https://danielbendix.github.io/glang)*

### Factorial function
```
fn fac(n: i64) -> i64 {
    guard n >= 2 else { return 1; }
    return n * fac(n - 1);
}
```

### Summing numbers in arrays
```
// Sum of a bounded array (pointer and size)
fn sumBounded(numbers: i64[]) -> i64 {
    var sum = 0;
    for n in numbers {
        sum += n;
    }
    return sum;
}

// Sum of an unbounded array and length (indexable pointer)
fn sumUnbounded(numbers: i64[!], count: i64) -> i64 {
    var sum = 0;
    for i in 0 ..< count {
        sum += numbers[i];
    }
    return sum;
}
```

### Reversing a linked list:
```
struct ListI64 {
    let value: i64;
    var next: ListI64*? = nil;
}

fn reverse(head: ListI64*?) -> ListI64*? {
    var head = head;
    var prev: ListI64*? = nil;

    while let current = head {
        let next = current@.next;
        current@.next = prev;
        prev = current;
        head = next;
    }

    return prev;
}
```


## Roadmap

### Features

- Support for strings. Either via a built-in type, or via overloaded string literals
- The ability to output a binary, and not just LLVM IR
- More work on structs:
    - Alignment and layout guarantees
    - Methods
- Globals, ensuring that the value of a global does not depend on itself through its initial value
- Static arrays, both local and global, and a type for them
- More data types:
    - Enums
    - Unions (tagged & untagged)    
    - Pointer unions (storing a type tag in the alignment bits of a pointer)
    - Non-reassignable tagged unions:
        - Cannot be reassigned to a different case
        - Cases will share alignment
        - But only use the memory required by the individual case
- Intrinsics, like cast, bitcast, sign extend, zero extend, truncate, etc
- Structured error handling, like in Swift, but always strongly typed
    - This needs to include a specification of the calling convention for throwing functions
- C-compatible FFI
- Extensions for types

In the not-too-distant future:
- Standard library
- Generics
- Protocols for bounding generics
- Opt-in dynamic dispatch through protocols
- A macro system of sorts

### Architecture

#### Mid-level IR

Currently the AST is the only IR before lowering to LLVM. The AST stores pointers for resolutions and types, and some nodes are "decorated" during type checking with wrapper nodes for operations like sign extension. Nodes in the AST will have reasonable locality due to the use of a bump-pointer allocator, but any added decorator nodes means that references will be "jumping around" much more during codegen.

By implementing a new IR between AST and lowering to LLVM IR, the following can be achieved:
- Smaller AST nodes, due to no type information, so better locality when traversing the AST.
- Any implicit "decorators" can be added during lowering to Mid-level IR, so that data access will exhibit better locality during codegen.
- An IR that is closer to the level of LLVM IR would allow for:
    1. Less complexity in the code generation phase.
    2. Generating LLVM IR that is more conducive to lowering to assembly.
