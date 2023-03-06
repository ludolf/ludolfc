
# LudolfC Programming Language

LudolfC is a simple programming language for learning concepts of structured, 
    object-oriented and functional programming.
    
Check it out online at https://ludolfc.github.io

<img src="logo.svg" style="width: 20rem; float: right;" align="right">

## Language Overview

### Assignment
```
name := value
```

Variable names can contain all word characters, digits and underscore `_`. 
A varible name must not start with a digit.

Unicode characters `ěščřžťďýáíéúůüöäňñĚŠČŘŽŤĎÝÁÍÉÚŮÜÖÄŇÑß` are allowed.

Variable names are case-sensitive.

### Data types

LudolfC is a dynamically-typed language. The type of a variable can differ with 
each assignment.

#### Numbers
```
integer := 123
decimal := 123.45

four  := 5 + 4 / 2 + -3
three := -0.5 + 1.25 + 2.25
```

| Feature   | Operation      | Return type | Example           |
| --------- | -------------- | ----------- | ----------------- |
| `plus`    | addition       | Number      | `1.plus(2) = 3`   |
| `minus`   | subtraction    | Number      | `2.minus(1) = 1`  |
| `mult`    | multiplication | Number      | `2.mult(3) = 6`   |
| `div`     | division       | Number      | `4.div(2) = 2`    |
| `mod`     | modulo         | Number      | `5.mod(3) = 2`    |
| `neg`     | negation       | Number      | `1.neg() = -1`    |
| `sum`     | sum            | Number      | `1.sum(2, 3) = 6` |
| `round`   | rounding       | Number      | `1.5.round() = 2` |
| `floor`   | floor          | Number      | `1.5.floor() = 1` |
| `ceil`    | ceiling        | Number      | `1.2.ceil() = 2`  |

#### Strings
```
string1 := "Hello World!"
string2 := 'Hello World!'
string3 := “Hello World!”

empty := ""
abcde := "ab" + 'c' + “de”
```

| Feature   | Operation      | Return type | Example                      |
| --------- | -------------- | ----------- | ---------------------------- |
| `concat`  | concatenation  | String      | `"ab".concat("c") = "abc"`   |
| `charAt`  | char at index  | String      | `"abc".charAt(1) = "b"`      |
| `sub`     | substring      | String      | `"abc".sub(1,2) = "b"`       |

| Property  | Value          | Data type   | Example          |
| --------- | -------------- | ----------- | ---------------- |
| `size`    | size           | Number      | `"abc".size = 3` |

#### Boolean
```
t := true | false
f := true & false

t := 1 <= 2
t := 1 != 2
```

| Feature   | Operation             | Return type | Example                     |
| --------- | --------------------- | ----------- | --------------------------- |
| `nand`    | logical negation      | Boolean     | `true.neg() = false`        |
| `and`     | logical conjunction   | Boolean     | `true.and(false) = false`   |
| `or`      | logical disjunction   | Boolean     | `true.or(false) = true`     |
| `xor`     | exclusive disjunction | Boolean     | `true.xor(true) = false`    |
| `nand`    | alternative denial    | Boolean     | `true.nand(true) = false`   |

#### Void
Void is a special data type with an empty value. Void is a result of statement 
executions or calls of empty functions.

### Arrays

Arrays are heterogeneous collections of indexed elements whose index starts at 
zero.

```
arr := []
arr := [1]
arr := [1, 2, 3]
arr := [1, [2, 3]]
arr := [[1], [2, 3]]
arr := [1, "x", true]

// [1, 123, true]
arr[1] := 123

// [1, [123], true]
arr[1] := [123]

// [1, [999], true]
arr[1,0] := 999

// 3
arr.size

// 1
arr[1].size

// true
[1,2] = [1,2]

// false
[1,2] = [2,1]

// true
[1] + [2,3] = [1,2,3]
```

| Feature   | Operation      | Return type | Example                         |
| --------- | -------------- | ----------- | ------------------------------- |
| `concat`  | concatenation  | Array       | `[1,2].concat([3]) = [1,2,3]`   |

| Property  | Value          | Data type   | Example            |
| --------- | -------------- | ----------- | ------------------ |
| `size`    | size           | Number      | `[1,2,3].size = 3` |

### Conditionals

Conditionals are boolean-condition-controlled branches of the program.

```
if condition {
    // body for condition is true
}

if condition {
    // body for condition is true
} else {
    // body for condition is false
}
```

Conditions must be of type Boolean:
```
if true {}
if false {} else {}
```

The body of conditionals is a sequence of instructions:
```
a := 0
b := 0

if a <= 0 {
    a := a + 1
    b := 1
} 

if a <= 0 {
    a := a + 1
    b := 2
}
else {
    a := a - 1
    b := 3
}

// 3
a + b
```

Variables created inside a body are scoped to the life time of that body.

### Loops

Loops are sequences of instructions that are continually repeated while 
a condition is met.

```
while condition {
    // body to repeat
}

i := 1
while i <= 10 {
    i := i * 2
}
// i = 16
```

Variables created inside a body are scoped to the life time of that body.

### Functions

Functions are callable sub-programs with zero or more named parameters.

The last statement is returned as a result of the function call.

```
empty := (){}
void := empty()

identity := (x){x}
one := identity(1)

addition := (x,y){ x + y }
three := addition(1,2)
```

Variables created inside a function are scoped to the life time of that 
function:
```
func := (x,y){
    res := x + y
    res * 2
}
six := func(1,2)
// 'res' does not exist here
```

Functions are first-class citizens:
```
f := (){(){1}}
f()()

x := 1
f := (a){(){x+a()}}
g := f((){x})
g()
```

### Objects

Object are heterogeneous structures of named attributes.

```
obj := {}
obj := { a: 1 }
obj := { a: 1, b: "B" }
obj := { a: 1, b: "B", t: true }
obj := { a: 1, b: "B", t: true, arr: [1,2,3] }
obj := { a: 1, b: "B", t: true, arr: [1,2,3], f: (x){x*2} }
obj := { a: 1, b: "B", t: true, arr: [1,2,3], f: (x){x*2}, o:{a:5} }

// 1
obj.a

// 2
obj.arr[1]

// 246
obj.f(123)

// 5
obj.o.a
```

Object's attributes are available inside member functions:
```
a := 0
obj := {
    a: 1,
    f: (){ a + 1 }
}

// 2
obj.f()
```

Everything is an object:

```
1.plus(2)

"Hello".concat("World")

false.or(true)

[1,2].eq([1,2])
```

### Keywords

All keywords are case-insensitive!

`true`, `false`, `if`, `else`, `while`

## Internationalization

In order to be used in different native languages, LudolfC has several mutations of keywords and standard attributes. Other mutations are forseen in the future.

| Keyword   | German (de) | Czech (cs) |
| --------- | ----------- | ---------- |
| `true`    | `wahr`      | `pravda`   |
| `false`   | `unwahr`    | `nepravda` |
| `if`      | `falls`     | `pokud`    |
| `else`    | `sonst`     | `jinak`    |
| `while`   | `solange`   | `dokud`    |

| Attribute   | German (de) | Czech (cs)    |
| ----------- | ----------- | ------------- |
| `[].size`   | `[].größe`  | `[].velikost` |

## Interpreter

LudolfC comes along with a JavaScript interpreter:

```
npm i ludolfc
```

```
import {LudolfC, lang} from 'ludolfc'

var imports = {
    inc: new lang.NativeFunction(x => new lang.Number(x.value + 1)),
    dec: new lang.NativeFunction(x => new lang.Number(x.value - 1)),
}

var ludolfC = new LudolfC(imports)

var result = ludolfC.execute(`
    i := 1
    a := inc(i)
    b := dec(i)
    i + a + b
`)

console.log(result.value)   // 3
```

A web-based interpreter is to be found in [dist/](dist/).

## Examples

```
insertionSort := (arr) {
  n := arr.size
  i := 1
  while i < n {
    c := arr[i]
    j := i - 1 
    while j > -1 & c < arr[j] {
      arr[j + 1] := arr[j]
      j := j - 1
    }
    arr[j + 1] := c
    i := i + 1
  }
  arr
}

insertionSort([5,3,2,1,4])
```

```
o := 1
(o){(o){o}((o){o})}(o)(o)
```

## Build
```
npm run build
```

## License

[GPL-3.0](LICENSE)
