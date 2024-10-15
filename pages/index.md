<div align="center">

# [G](https://github.com/danielbendix/glang)

A low-level programming language inspired by C & Swift.

</div>

## Examples

<style>
pre { white-space: pre-wrap; font-family: monospace; color: #cad3f5; background-color: #24273a; }
body { font-family: monospace; color: #cad3f5; background-color: #24273a; }
* { font-size: 1em; }
.Statement { color: #c6a0f6; }
.Comment { color: #5b6078; }
.Type { color: #8aadf4; }
.Keyword { color: #f5bde6; }
.Conditional { color: #ed8796; }
.StorageClass { color: #eed49f; }
</style>

### Factorial function
<pre>
<span class="Keyword">fn</span> fac(n: <span class="Type">i64</span>) -&gt; <span class="Type">i64</span> {
    <span class="Conditional">guard</span> n &gt;= <span class="Number">2</span> <span class="Conditional">else</span> { <span class="Statement">return</span> <span class="Number">1</span>; }
    <span class="Statement">return</span> n * fac(n - <span class="Number">1</span>);
}
</pre>

### Summing numbers in arrays
<pre>
<span class="Comment">// Sum of a bounded array (pointer and size)</span>
<span class="Keyword">fn</span> sumBounded(numbers: <span class="Type">i64</span>[]) -&gt; <span class="Type">i64</span> {
    <span class="Keyword">var</span> sum = <span class="Number">0</span>;
    <span class="Conditional">for</span> n <span class="Conditional">in</span> numbers {
        sum += n;
    }
    <span class="Statement">return</span> sum;
}

<span class="Comment">// Sum of an unbounded array and length (indexable pointer)</span>
<span class="Keyword">fn</span> sumUnbounded(numbers: <span class="Type">i64</span>[!], count: <span class="Type">i64</span>) -&gt; <span class="Type">i64</span> {
    <span class="Keyword">var</span> sum = <span class="Number">0</span>;
    <span class="Conditional">for</span> i <span class="Conditional">in</span> <span class="Number">0</span> ..&lt; count {
        sum += numbers[i];
    }
    <span class="Statement">return</span> sum;
}
</pre>

### Reversing a linked list:
<pre>
<span class="StorageClass">struct</span> ListI64 {
    <span class="Keyword">let</span> value: <span class="Type">i64</span>;
    <span class="Keyword">var</span> next: ListI64*? = <span class="Keyword">nil</span>;
}

<span class="Keyword">fn</span> reverse(head: ListI64*?) -&gt; ListI64*? {
    <span class="Keyword">var</span> head = head;
    <span class="Keyword">var</span> prev: ListI64*? = <span class="Keyword">nil</span>;

    <span class="Conditional">while</span> <span class="Keyword">let</span> current = head {
        <span class="Keyword">let</span> next = current@.next;
        current@.next = prev;
        prev = current;
        head = next;
    }

    <span class="Statement">return</span> prev;
}
</pre>


## Roadmap

### Features

- Support for strings. Either via a built-in type, or via overloaded string literals
- The ability to output a binary, and not just LLVM IR
- More work on structs:
    - Cycle detection in struct members
    - Alignment and layout guarantees
    - Methods
- Better global handling:
    - Initializing globals with constant values at compile-time
    - Analyzing dependencies through functions
- Constant folding during type checking:
    - Ensuring that numbers do not exceed their type bounds
    - Ensuring that constants operands are valid, e.g. no negative shift amounts or negative indices in bounded arrays
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
