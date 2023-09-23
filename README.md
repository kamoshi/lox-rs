# Loxy
Another Rust implementation of the Lox programming language

## AST interpreter
### Supported types
- Nil
- Boolean
- Number (float64)
- String (UTF8 string),
- Functions (first class)

### Features
- [x] repl
  - [x] expressions
  - [x] statements
- [x] print
- [x] variables
- [x] scope
- [x] conditionals
- [x] logic
- [x] while
- [x] for
- [x] functions
  - [x] closures
  - [x] lambdas
- [ ] classes


## Examples

```c
fun makeCounter() {
  var i = 0;
  fun count() {
    i = i + 1;
    print(i);
  }

  return count;
}

var counter = makeCounter();
counter(); // 1
counter(); // 2
```

```c
fun thrice(fn) {
  for (var i = 1; i <= 3; i = i + 1) {
    fn(i);
  }
}

thrice(fun (a) {
  print(a);
});
// 1
// 2
// 3

```

