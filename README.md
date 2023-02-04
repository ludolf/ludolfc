
# LudolfC Programming Language

LudolfC is a simple programming language for learning concepts of structured, 
    object-oriented and functional programming.

<img src="logo.svg" style="width: 20rem; float: right;" align="right">

## Language Syntax

### Assignment
```
name := value
```

Variable names can contain all word characters, digits and underscore `_`. 
A varible name must not start with a digit.

Unicode characters `ěščřžťďýáíéúůüöäñĚŠČŘŽŤĎÝÁÍÉÚŮÜÖÄÑß` are allowed.

Variable names are case-insensitive.

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

#### Strings
```
string1 := "Hello World!"
string2 := 'Hello World!'
string3 := “Hello World!”

empty := ""
abcde := "ab" + 'c' + “de”
```

#### Boolean
```
t := true | false
f := true & false

t := 1 <= 2
t := 1 != 2
```

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
```

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
three := fn(1,2)
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

### Keywords
`true`, `false`, `if`, `else`, `while`

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
        current := arr[i]
        j := i - 1 
        while j > -1 & current < arr[j] {
            arr[j + 1] := arr[j]
            j := j - 1
        }
        arr[j + 1] := current
        i := i + 1
    }
    arr
}

insertionSort([8,5,6,3,2,1,7,9,4])
```

```
// prints 1
o  := 1
oo := (o){o}
(o){(o){o}((ooo){oo(ooo)}((o){o}(o)))}(oo(o))
```

## Build
```
npm run build
```
