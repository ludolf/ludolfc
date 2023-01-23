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

test('interpret expression numbers ops', () => {
  const ast = parser.parse('-1 + 2 - -3')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
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

test('parser expression array expression', () => {
  const ast = parser.parse('[1+2][0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('parser expression array expression resolved', () => {
  const ast = parser.parse('a:=1\nb:=[a]\na:=2\nb[0]')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('parser expression array expression resolved #2', () => {
  const ast = parser.parse('a:=1\nf:=(){a:=2}\nb:=[f()]\na=2')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('parser expression array expression resolved', () => {
  const ast = parser.parse('a:=1\nb:={a:a}\na:=2\nb.a')
  const result = interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('parser expression object expression resolved #2', () => {
  const ast = parser.parse('a:=1\nf:=(){a:=2}\nb:={a:f()}\na=2')
  const result = interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
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

test('interpret assignment fuction def body', () => {
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