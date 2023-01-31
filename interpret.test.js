const Parser = require('./parser')
const parser = new Parser()
const Interpret = require('./interpret')
const interpret = new Interpret()

test('interpret expression number simplest', () => {
  const ast = parser.parse('1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression number biop', () => {
  const ast = parser.parse('1 + 2')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression number biop #2', () => {
  const ast = parser.parse('1 + 2 * 3')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(7)
})

test('interpret expression number biop #3', () => {
  const ast = parser.parse('(1 + 2) * 3')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(9)
})

test('interpret expression number uniop', () => {
  const ast = parser.parse('-1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-1)
})

test('interpret expression number uniop #2', () => {
  const ast = parser.parse('--1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression number uniop #3', () => {
  const ast = parser.parse('!!(-1 >= -1)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression number uniop #4', () => {
  const ast = parser.parse('-1 - 1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-2)
})

test('interpret expression numbers ops', () => {
  const ast = parser.parse('-1 + 2 - -3')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret expression numbers ops #2', () => {
  const ast = parser.parse('3 / 9 * 3')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression numbers ops #3', () => {
  const ast = parser.parse('1--1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression numbers ops #4', () => {
  const ast = parser.parse('1+-1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0)
})

test('interpret expression numbers ops #5', () => {
  const ast = parser.parse('-1-1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-2)
})

test('interpret expression numbers ops block', () => {
  const ast = parser.parse('-1 + (2 - -3)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret expression group', () => {
  const ast = parser.parse('(1)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression group #2', () => {
  const ast = parser.parse('((123))')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(123)
})

test('interpret expression group #3', () => {
  const ast = parser.parse('((123) + 12) + 21 - ((5))')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(151)
})

test('interpret expression group #4', () => {
  const ast = parser.parse('1 + (5 * 3)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(16)
})

test('interpret expression group #5', () => {
  const ast = parser.parse('(1 + 5) * 3')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(18)
})

test('interpret expression group #6', () => {
  const ast = parser.parse('2 * 5 + 3')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(13)
})

test('interpret expression group #7', () => {
  const ast = parser.parse('2 * (5 + 3)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(16)
})

test('interpret expression group #8', () => {
  const ast = parser.parse('((1)+((2)))')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression group #9', () => {
  const ast = parser.parse('((1)+((-2)))')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-1)
})

test('interpret expression object', () => {
  const ast = parser.parse('{a:1}.a')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression object #2', () => {
  const ast = parser.parse('{a:1,b:"x"}.b')
  const result = interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('x')
})

test('interpret expression object complex', () => {
  const ast = parser.parse('{a:1,b:"x",c:{x:true},d:[3,4]}.d[1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret expression object complex #2', () => {
  const ast = parser.parse('{a:1,b:"x",c:{x:true},d:[3,4]}.c.x')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression array access', () => {
  const ast = parser.parse('[2][0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression array access #1', () => {
  const ast = parser.parse('[1,2][1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression array access #2', () => {
  const ast = parser.parse('[123,"abc",true][0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(123)
})

test('interpret expression array access #3', () => {
  const ast = parser.parse('[123,"abc",true][1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('abc')
})

test('interpret expression array access #4', () => {
  const ast = parser.parse('[123,"abc",true][2]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression array dimensions access', () => {
  const ast = parser.parse('[[1],[2,3]][1,0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression array complex access', () => {
  const ast = parser.parse('[1,[2],{a:3},[{b:[4+5]}]][3][0].b[0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(9)
})

test('interpret r expression array expression', () => {
  const ast = parser.parse('[1+2][0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret r expression array expression resolved', () => {
  const ast = parser.parse('a:=1\nb:=[a]\na:=2\nb[0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret r expression array expression resolved #2', () => {
  const ast = parser.parse('a:=1\nf:=(){a:=2}\nb:=[f()]\na=2')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret r expression array expression scoped', () => {
  const ast = parser.parse('a:=1\nf:=(a){a:=2}\nb:=[f(3)]\na=1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret r expression array expression resolved', () => {
  const ast = parser.parse('a:=1\nb:={a:a}\na:=2\nb.a')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret r expression object expression resolved #2', () => {
  const ast = parser.parse('a:=1\nf:=(){a:=2}\nb:={a:f()}\na=2')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret r expression object expression scoped', () => {
  const ast = parser.parse('a:=1\nf:=(a){a:=2}\nb:={a:f(3)}\na=1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression function', () => {
  const ast = parser.parse('(){}()')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret expression function #2', () => {
  const ast = parser.parse('(x){x}(1)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #3', () => {
  const ast = parser.parse('(x,y){x+y}(1,2)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #4', () => {
  const ast = parser.parse('(){}((){})')
  expect(() => interpret.execute(ast)).toThrow()  // argument mishmash
})

test('interpret expression function #5', () => {
  const ast = parser.parse('(x){x}((){1}())')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #6', () => {
  const ast = parser.parse('(x){x}((){1}())')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #7', () => {
  const ast = parser.parse('(){}(1+2)')
  expect(() => interpret.execute(ast)).toThrow()  // argument mishmash
})

test('interpret expression function #8', () => {
  const ast = parser.parse('(x){x}(1+2)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #9', () => {
  const ast = parser.parse('(x){x}((){1}())')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #10', () => {
  const ast = parser.parse('(x){x}(1+(){2}())')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #11', () => {
  const ast = parser.parse('(x){x}((){2}()+3)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(5)
})

test('interpret expression function #12', () => {
  const ast = parser.parse('(x){x}(1+(){2}()+3)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret expression function #13', () => {
  const ast = parser.parse('(){(){}()}()')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret expression function #14', () => {
  const ast = parser.parse('(){(){1}()}()+(){(){2}()}()')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #15', () => {
  const ast = parser.parse('(){(){(){(){1}()}()}()}()')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #16', () => {
  const ast = parser.parse('1+(){2+(){3+(){(4+5)}()+6}()+7}()+8')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(36)
})

test('interpret expression function #17', () => {
  const ast = parser.parse('(x){x}((1))')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #18', () => {
  const ast = parser.parse('(x){(y){(){x+y}()}(x)}(1)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression function #19', () => {
  const ast = parser.parse('(x){(y){(){x+y}()}(2)}(1)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #20', () => {
  const ast = parser.parse('4+(x){(y){(){x+y}()}(2)}(1)+5')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(12)
})

test('interpret expression function #21', () => {
  const ast = parser.parse('(x){(){1}()+x+(){2}()}((){3}()+(){4}())')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret expression function #22', () => {
  const ast = parser.parse('(f,g){f()+g()}((){1},(){2})')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #23', () => {
  const ast = parser.parse('(f,g){f(1)+g(2)}((x){3+x},(x){x+4})')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret expression function #24', () => {
  const ast = parser.parse('(x,y){x+y}((){1}(),(){2}()+(){3}())')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret r expression native function', () => {
  const ast = parser.parse('1.plus(2)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret r expression native function #2', () => {
  const ast = parser.parse('1.plus(2+3)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret r expression native function #3', () => {
  const ast = parser.parse('1.plus(2.plus(3))')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret r expression native function #4', () => {
  const ast = parser.parse('1.plus(2.plus(3)+4)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret r expression native function #5', () => {
  const ast = parser.parse('1.plus(2.plus(3)+4)+5')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(15)
})

test('interpret r expression native function #6', () => {
  const ast = parser.parse('6+1.plus(2.plus(3)+4)+5')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(21)
})

test('interpret r expression native function #7', () => {
  const ast = parser.parse('4+2.mult(3)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret r expression native function #8', () => {
  const ast = parser.parse('2.mult(3)+1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(7)
})

test('interpret assignment number simplest', () => {
  const ast = parser.parse('a := 1\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret assignment number simplest #2', () => {
  const ast = parser.parse('a := 1 / 2\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0.5)
})

test('interpret assignment with object access', () => {
  const ast = parser.parse('o := {a:1}\no.a := 2\no.a')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret assignment with object access #2', () => {
  const ast = parser.parse('o := {a:{b:1}}\no.a.b := 2\no.a.b')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret assignment with object access #3', () => {
  const ast = parser.parse('o := {a:{b:1},b:3}\no.a.b := 2\no.b')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret assignment with array access #3', () => {
  const ast = parser.parse('a := [1,2,3]\na[1] := 4\na[1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret assignment with array access #4', () => {
  const ast = parser.parse('a := [1,2,3]\na[1] := 4\na[0]+a[1]+a[2]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(8)
})

test('interpret assignment with array access #5', () => {
  const ast = parser.parse('a := [[1],[2,3]]\na[1,0] := 4\na[1,0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret assignment with array access #6', () => {
  const ast = parser.parse('a := [[1],[2,3]]\na[1,0] := 4\na[0,0]+a[1,0]+a[1,1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(8)
})

test('interpret assignment with array access #7', () => {
  const ast = parser.parse('a := [[1],[2,3]]\na[1][0] := 4\na[0,0]+a[1,0]+a[1,1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(8)
})

test('interpret assignment function def body', () => {
  const ast = parser.parse('f := (){}\nf()')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret assignment function def body #2', () => {
  const ast = parser.parse('f := (a,b){x := a + b\nx + 1}\nf(1,2)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret statement multiple', () => {
  const ast = parser.parse('a := 1\na + 2')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret statement multiple #2', () => {
  const ast = parser.parse('a := 1\nb := 2\nc := a + b\nc')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})


test('interpret statement while', () => {
  const ast = parser.parse('while false {}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #2', () => {
  const ast = parser.parse('while (false) {}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #3', () => {
  const ast = parser.parse('while 1 > 2 {}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #4', () => {
  const ast = parser.parse('while ((){false}()) {}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #5', () => {
  const ast = parser.parse('while ((){true}() & false) {}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #6', () => {
  const ast = parser.parse('while false {1}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #7', () => {
  const ast = parser.parse('while false {1}\nwhile false {2}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #8', () => {
  const ast = parser.parse('a := 0\nwhile a < 10 { a := a + 1 }\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #9', () => {
  const ast = parser.parse('a := 0\nb := 0\nwhile a < 10 { b := b + 1\na := a + 2 }\nb')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(5)
})

test('interpret statement while #10', () => {
  const ast = parser.parse('a := 0\nb := 0\nwhile a < 10 { b := b + 1\na := a + 2 }\nb + a')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(15)
})

test('interpret statement while #11', () => {
  const ast = parser.parse('a := 0\nb := 0\nwhile a < 10 { b := b + 1\na := a + 2 }\n\nwhile b < 10 { b := b + 1\na := a + 2 }\nb + a')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(30)
})

test('interpret statement while #12', () => {
  const ast = parser.parse('a := 0\nwhile ((){a < 10}()) { a := a + 1 }\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #13', () => {
  const ast = parser.parse('a := 0\nwhile ((){(){a < 10}()}()) { a := a + 1 }\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #14', () => {
  const ast = parser.parse('a := 0\nwhile a < 10 | a < 30 { a := a + 1 }\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(30)
})

test('interpret statement while #15', () => {
  const ast = parser.parse('a := 0\nwhile a < 10 & a < 30 { a := a + 1 }\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #16', () => {
  const ast = parser.parse('a := 0\nwhile ((a < 10)) { a := a + 1 }\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #17', () => {
  const ast = parser.parse('x:=0\ni:=0\nwhile i < 10 { j:=0\nwhile j < 10 { j:=j+1\nx:=x+1 }\ni:=i+1 }\nx')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(100)
})

test('interpret if', () => {
  const ast = parser.parse('a:=1\nif true { a:=2 }\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if else', () => {
  const ast = parser.parse('a:=1\nif true {a:=2} else {a:=3}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if else #2', () => {
  const ast = parser.parse('a:=1\nif false {a:=2} else {a:=3}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if else #3', () => {
  const ast = parser.parse('x:=0\na:=1\nb:=2\nif true {x:=a} else {x:=b}\nx')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if else #4', () => {
  const ast = parser.parse('a:=1\nif 2 >= 1 {a:=2} else {a:=3}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if else #5', () => {
  const ast = parser.parse('a:=0\nif true {a:=1}\nif false {a:=2} else {a:=3}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if else #6', () => {
  const ast = parser.parse('a:=0\nif a < 1 {a:=1}\nif a >= 1 {a:=2} else {a:=3}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if else #7', () => {
  const ast = parser.parse('a := 0\nb := 0\nif a < 10 { b := b + 1\na := a + 2 }\nb+a')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if else #8', () => {
  const ast = parser.parse('a:=0\nif ((){true}()) {a:=1}\nelse {a:=2}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if else #9', () => {
  const ast = parser.parse('a:=0\nif ((x){1=1}(1)) {a:=1}\nelse {a:=3}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if else #10', () => {
  const ast = parser.parse('a:=0\nif (1<=1) {a:=1}\nelse {a:=2}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret function recursion', () => {
  const ast = parser.parse('f := (){ a:=a+1\nif a < 10 { f() }}\na:=1\nf()\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret function double call', () => {
  const ast = parser.parse('inc:=(){a:=a+1}\nf:=(){while a < 10 { inc() }}\na:=1\nf()\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret function double call #2', () => {
  const ast = parser.parse('inc:=(){a:=a+1}\nf:=(){while a < 10 { if a < 22 {inc()} }}\na:=1\nf()\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret object self', () => {
  const ast = parser.parse('a:=0\no:={a:1,f:(){$.a:=2\na:=3}}\no.f()\no.a')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret object self #2', () => {
  const ast = parser.parse('a:=0\no:={a:1,f:(){$.a:=2\na:=3}}\no.f()\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret string concat', () => {
  const ast = parser.parse('"" + ""')
  const result = interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret string concat #2', () => {
  const ast = parser.parse('"a" + ""')
  const result = interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('a')
})

test('interpret string concat #3', () => {
  const ast = parser.parse('"a" + "b"')
  const result = interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('ab')
})

test('interpret string concat #4', () => {
  const ast = parser.parse('"a" + "bc" + "" + "d"')
  const result = interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('abcd')
})

test('interpret array concat', () => {
  const ast = parser.parse('[] + []')
  const result = interpret.execute(ast)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(0)
})

test('interpret array concat #2', () => {
  const ast = parser.parse('[1] + []')
  const result = interpret.execute(ast)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(1)
  expect(result.value[0].value).toBe(1)
})

test('interpret array concat #3', () => {
  const ast = parser.parse('[1] + [2]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(2)
  expect(result.value[0].value).toBe(1)
  expect(result.value[1].value).toBe(2)
})

test('interpret array concat #4', () => {
  const ast = parser.parse('[1] + [2,3] + [] + [4]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(4)
  expect(result.value[0].value).toBe(1)
  expect(result.value[1].value).toBe(2)
  expect(result.value[2].value).toBe(3)
  expect(result.value[3].value).toBe(4)
})

test('interpret array size', () => {
  const ast = parser.parse('[1].size')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret array size #2', () => {
  const ast = parser.parse('[].size')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0)
})

test('interpret array size #3', () => {
  const ast = parser.parse('[1,2].size')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret array size #4', () => {
  const ast = parser.parse('[1,2].size + 1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret array size #5', () => {
  const ast = parser.parse('[1,2].velikost')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret string size', () => {
  const ast = parser.parse('"a".size')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret string size #2', () => {
  const ast = parser.parse('"".size')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0)
})

test('interpret string size #3', () => {
  const ast = parser.parse('"ab".size')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret string size #4', () => {
  const ast = parser.parse('"ab".size + 1')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret string size #5', () => {
  const ast = parser.parse('"ab".velikost')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret array eq', () => {
  const ast = parser.parse('[1] = [1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array eq #2', () => {
  const ast = parser.parse('[] = []')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array eq #3', () => {
  const ast = parser.parse('[1,2] = [1,2]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array eq #4', () => {
  const ast = parser.parse('[1,2] = [1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret array eq #5', () => {
  const ast = parser.parse('[1,2] = [2,1]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret array eq #6', () => {
  const ast = parser.parse('[[1],2] = [[1],2]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array eq #7', () => {
  const ast = parser.parse('[[1,3],2] = [[1],2]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret object eq', () => { // all objects are unique => always false
  const ast = parser.parse('{} = {}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret object eq #2', () => { // all objects are unique => always false
  const ast = parser.parse('{a:1} = {a:1}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret function eq', () => {
  const ast = parser.parse('(){} = (){}')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret void eq', () => {
  const ast = parser.parse('(){}() = (){}()')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret complex eq', () => {
  const ast = parser.parse('[1,{}] = [1,{}]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret complex eq #2', () => {
  const ast = parser.parse('[[1,["a"],[1,2,3/2,[]]]] = [[1,["a"],[1,2,3/2,[]]]]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret redefine eq', () => {
  const ast = parser.parse('o:={eq:(x){true}}\no.eq({})')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret redefine eq #2', () => {
  const ast = parser.parse('o:={id:1,eq:(x){x.id=id}}\no.eq({id:1})')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret redefine eq #3', () => {
  const ast = parser.parse('o:={id:1,eq:(x){x.id=id}}\no.eq({id:2})')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret scoped variable', () => {
  const ast = parser.parse('f:=(a){a:=1\na}\nf(2)')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret scoped variable #2', () => {
  const ast = parser.parse('a:=3\nf:=(a){a:=1}\nf(2)\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret scoped variable #3', () => {
  const ast = parser.parse('a:=3\nf:=(){a:=1}\nf()\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret scoped variable #4', () => {
  const ast = parser.parse('a:=3\nf:=(a){a:=a}\nf(2)\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret scoped variable #5', () => {
  const ast = parser.parse('a:=3\nf:=(a){}\nf(2)\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret scoped variable #6', () => {
  const ast = parser.parse('b:=2\nf:=(a){a:=1}\nf(b)\nb')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret scoped variable #7', () => {
  const ast = parser.parse('if true {a:=2} else {a:=3}\ntrue')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret scoped variable #8', () => {
  const ast = parser.parse('a:=1\nif a=1 {a:=2} else {a:=3}\na')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret scoped variable error', () => {
  const ast = parser.parse('f:=(){a:=1}\nf()\na')
  expect(() => interpret.execute(ast)).toThrow()  // a does not exist in the global scope
})

test('interpret scoped variable error #2', () => {
  const ast = parser.parse('f:=(a){a:=1}\nf(1)\na')
  expect(() => interpret.execute(ast)).toThrow()  // a does not exist in the global scope
})

test('interpret scoped variable error #3', () => {
  const ast = parser.parse('b:=1\nf:=(a){a:=1}\nf(b)\na')
  expect(() => interpret.execute(ast)).toThrow()  // a does not exist in the global scope
})

test('interpret scoped variable error #4', () => {
  const ast = parser.parse('if true {a:=1} else {a:=2}\na')
  expect(() => interpret.execute(ast)).toThrow()  // a does not exist in the global scope
})

test('interpret scoped variable error #5', () => {
  const ast = parser.parse('i:=0\nwhile i=0 {a:=1\ni:=2}\na')
  expect(() => interpret.execute(ast)).toThrow()  // a does not exist in the global scope
})