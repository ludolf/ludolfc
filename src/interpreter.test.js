const Parser = require('./parser')
const parser = new Parser()
const Interpret = require('./interpreter')
const interpret = new Interpret()

test('interpret expression number simplest', async () => {
  const ast = parser.parse('1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression number decimal', async () => {
  const ast = parser.parse('1.2')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1.2)
})

test('interpret expression number decimal #2', async () => {
  const ast = parser.parse('0.25')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0.25)
})

test('interpret expression number decimal #3', async () => {
  const ast = parser.parse('-1.2')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-1.2)
})

test('interpret expression number decimal #4', async () => {
  const ast = parser.parse('-0.25')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-0.25)
})

test('interpret expression number biop', async () => {
  const ast = parser.parse('1 + 2')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression number biop #2', async () => {
  const ast = parser.parse('1 + 2 * 3')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(7)
})

test('interpret expression number biop #3', async () => {
  const ast = parser.parse('(1 + 2) * 3')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(9)
})

test('interpret expression number biop #4', async () => {
  const ast = parser.parse('f:=(){1}\nf()+2')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression number uniop', async () => {
  const ast = parser.parse('-1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-1)
})

test('interpret expression number uniop #2', async () => {
  const ast = parser.parse('--1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression number uniop #3', async () => {
  const ast = parser.parse('!!(-1 >= -1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression number uniop #4', async () => {
  const ast = parser.parse('-1 - 1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-2)
})

test('interpret expression numbers ops', async () => {
  const ast = parser.parse('-1 + 2 - -3')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret expression numbers ops #2', async () => {
  const ast = parser.parse('3 / 9 * 3')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression numbers ops #3', async () => {
  const ast = parser.parse('1--1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression numbers ops #4', async () => {
  const ast = parser.parse('1+-1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0)
})

test('interpret expression numbers ops #5', async () => {
  const ast = parser.parse('-1-1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-2)
})

test('interpret expression numbers ops block', async () => {
  const ast = parser.parse('-1 + (2 - -3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret expression boolean op precedence', async () => {
  const ast = parser.parse('false | true & false')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret expression boolean op precedence #2', async () => {
  const ast = parser.parse('(false | true) & false')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret expression boolean op precedence #3', async () => {
  const ast = parser.parse('true | true & false')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret expression boolean op precedence #4', async () => {
  const ast = parser.parse('false | true & true')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression group', async () => {
  const ast = parser.parse('(1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression group #2', async () => {
  const ast = parser.parse('((123))')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(123)
})

test('interpret expression group #3', async () => {
  const ast = parser.parse('((123) + 12) + 21 - ((5))')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(151)
})

test('interpret expression group #4', async () => {
  const ast = parser.parse('1 + (5 * 3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(16)
})

test('interpret expression group #5', async () => {
  const ast = parser.parse('(1 + 5) * 3')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(18)
})

test('interpret expression group #6', async () => {
  const ast = parser.parse('2 * 5 + 3')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(13)
})

test('interpret expression group #7', async () => {
  const ast = parser.parse('2 * (5 + 3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(16)
})

test('interpret expression group #8', async () => {
  const ast = parser.parse('((1)+((2)))')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression group #9', async () => {
  const ast = parser.parse('((1)+((-2)))')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(-1)
})

test('interpret expression object', async () => {
  const ast = parser.parse('{a:1}.a')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression object #2', async () => {
  const ast = parser.parse('{a:1,b:"x"}.b')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('x')
})

test('interpret expression object complex', async () => {
  const ast = parser.parse('{a:1,b:"x",c:{x:true},d:[3,4]}.d[1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret expression object complex #2', async () => {
  const ast = parser.parse('{a:1,b:"x",c:{x:true},d:[3,4]}.c.x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression array access', async () => {
  const ast = parser.parse('[2][0]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression array access #1', async () => {
  const ast = parser.parse('[1,2][1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression array access #2', async () => {
  const ast = parser.parse('[123,"abc",true][0]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(123)
})

test('interpret expression array access #3', async () => {
  const ast = parser.parse('[123,"abc",true][1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('abc')
})

test('interpret expression array access #4', async () => {
  const ast = parser.parse('[123,"abc",true][2]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression array dimensions access', async () => {
  const ast = parser.parse('[[1],[2,3]][1,0]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression array complex access', async () => {
  const ast = parser.parse('[1,[2],{a:3},[{b:[4+5]}]][3][0].b[0]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(9)
})

test('interpret r expression array expression', async () => {
  const ast = parser.parse('[1+2][0]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret r expression array expression resolved', async () => {
  const ast = parser.parse('a:=1\nb:=[a]\na:=2\nb[0]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret r expression array expression resolved #2', async () => {
  const ast = parser.parse('a:=1\nf:=(){a:=2}\nb:=[f()]\na=2')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret r expression array expression scoped', async () => {
  const ast = parser.parse('a:=1\nf:=(a){a:=2}\nb:=[f(3)]\na=1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret r expression array expression resolved', async () => {
  const ast = parser.parse('a:=1\nb:={a:a}\na:=2\nb.a')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret r expression object expression resolved #2', async () => {
  const ast = parser.parse('a:=1\nf:=(){a:=2}\nb:={a:f()}\na=2')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret r expression object expression scoped', async () => {
  const ast = parser.parse('a:=1\nf:=(a){a:=2}\nb:={a:f(3)}\na=1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret expression function', async () => {
  const ast = parser.parse('(){}()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret expression function #2', async () => {
  const ast = parser.parse('(x){x}(1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #3', async () => {
  const ast = parser.parse('(x,y){x+y}(1,2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #4', () => {
  const ast = parser.parse('(){}((){})')
  expect(() => interpret.execute(ast)).rejects.toThrow()  // argument mishmash
})

test('interpret expression function #5', async () => {
  const ast = parser.parse('(x){x}((){1}())')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #6', async () => {
  const ast = parser.parse('(x){x}((){1}())')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #7', () => {
  const ast = parser.parse('(){}(1+2)')
  expect(() => interpret.execute(ast)).rejects.toThrow()  // argument mishmash
})

test('interpret expression function #8', async () => {
  const ast = parser.parse('(x){x}(1+2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #9', async () => {
  const ast = parser.parse('(x){x}((){1}())')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #10', async () => {
  const ast = parser.parse('(x){x}(1+(){2}())')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #11', async () => {
  const ast = parser.parse('(x){x}((){2}()+3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(5)
})

test('interpret expression function #12', async () => {
  const ast = parser.parse('(x){x}(1+(){2}()+3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret expression function #13', async () => {
  const ast = parser.parse('(){(){}()}()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret expression function #14', async () => {
  const ast = parser.parse('(){(){1}()}()+(){(){2}()}()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #15', async () => {
  const ast = parser.parse('(){(){(){(){1}()}()}()}()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #16', async () => {
  const ast = parser.parse('1+(){2+(){3+(){(4+5)}()+6}()+7}()+8')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(36)
})

test('interpret expression function #17', async () => {
  const ast = parser.parse('(x){x}((1))')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret expression function #18', async () => {
  const ast = parser.parse('(x){(y){(){x+y}()}(x)}(1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret expression function #19', async () => {
  const ast = parser.parse('(x){(y){(){x+y}()}(2)}(1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #20', async () => {
  const ast = parser.parse('4+(x){(y){(){x+y}()}(2)}(1)+5')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(12)
})

test('interpret expression function #21', async () => {
  const ast = parser.parse('(x){(){1}()+x+(){2}()}((){3}()+(){4}())')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret expression function #22', async () => {
  const ast = parser.parse('(f,g){f()+g()}((){1},(){2})')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret expression function #23', async () => {
  const ast = parser.parse('(f,g){f(1)+g(2)}((x){3+x},(x){x+4})')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret expression function #24', async () => {
  const ast = parser.parse('(x,y){x+y}((){1}(),(){2}()+(){3}())')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret r expression native function', async () => {
  const ast = parser.parse('1.plus(2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret r expression native function #2', async () => {
  const ast = parser.parse('1.plus(2+3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret r expression native function #3', async () => {
  const ast = parser.parse('1.plus(2.plus(3))')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret r expression native function #4', async () => {
  const ast = parser.parse('1.plus(2.plus(3)+4)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret r expression native function #5', async () => {
  const ast = parser.parse('1.plus(2.plus(3)+4)+5')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(15)
})

test('interpret r expression native function #6', async () => {
  const ast = parser.parse('6+1.plus(2.plus(3)+4)+5')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(21)
})

test('interpret r expression native function #7', async () => {
  const ast = parser.parse('4+2.mult(3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret r expression native function #8', async () => {
  const ast = parser.parse('2.mult(3)+1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(7)
})

test('interpret assignment number simplest', async () => {
  const ast = parser.parse('a := 1\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret assignment number simplest #2', async () => {
  const ast = parser.parse('a := 1 / 2\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0.5)
})

test('interpret assignment with object access', async () => {
  const ast = parser.parse('o := {a:1}\no.a := 2\no.a')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret assignment with object access #2', async () => {
  const ast = parser.parse('o := {a:{b:1}}\no.a.b := 2\no.a.b')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret assignment with object access #3', async () => {
  const ast = parser.parse('o := {a:{b:1},b:3}\no.a.b := 2\no.b')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret assignment with array access #3', async () => {
  const ast = parser.parse('a := [1,2,3]\na[1] := 4\na[1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret assignment with array access #4', async () => {
  const ast = parser.parse('a := [1,2,3]\na[1] := 4\na[0]+a[1]+a[2]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(8)
})

test('interpret assignment with array access #5', async () => {
  const ast = parser.parse('a := [[1],[2,3]]\na[1,0] := 4\na[1,0]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret assignment with array access #6', async () => {
  const ast = parser.parse('a := [[1],[2,3]]\na[1,0] := 4\na[0,0]+a[1,0]+a[1,1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(8)
})

test('interpret assignment with array access #7', async () => {
  const ast = parser.parse('a := [[1],[2,3]]\na[1][0] := 4\na[0,0]+a[1,0]+a[1,1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(8)
})

test('interpret assignment function def body', async () => {
  const ast = parser.parse('f := (){}\nf()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret assignment function def body #2', async () => {
  const ast = parser.parse('f := (a,b){x := a + b\nx + 1}\nf(1,2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret statement multiple', async () => {
  const ast = parser.parse('a := 1\na + 2')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret statement multiple #2', async () => {
  const ast = parser.parse('a := 1\nb := 2\nc := a + b\nc')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})


test('interpret statement while', async () => {
  const ast = parser.parse('while false {}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #2', async () => {
  const ast = parser.parse('while (false) {}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #3', async () => {
  const ast = parser.parse('while 1 > 2 {}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #4', async () => {
  const ast = parser.parse('while ((){false}()) {}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #5', async () => {
  const ast = parser.parse('while ((){true}() & false) {}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #6', async () => {
  const ast = parser.parse('while false {1}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #7', async () => {
  const ast = parser.parse('while false {1}\nwhile false {2}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('VOID')
})

test('interpret statement while #8', async () => {
  const ast = parser.parse('a := 0\nwhile a < 10 { a := a + 1 }\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #9', async () => {
  const ast = parser.parse('a := 0\nb := 0\nwhile a < 10 { b := b + 1\na := a + 2 }\nb')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(5)
})

test('interpret statement while #10', async () => {
  const ast = parser.parse('a := 0\nb := 0\nwhile a < 10 { b := b + 1\na := a + 2 }\nb + a')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(15)
})

test('interpret statement while #11', async () => {
  const ast = parser.parse('a := 0\nb := 0\nwhile a < 10 { b := b + 1\na := a + 2 }\n\nwhile b < 10 { b := b + 1\na := a + 2 }\nb + a')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(30)
})

test('interpret statement while #12', async () => {
  const ast = parser.parse('a := 0\nwhile ((){a < 10}()) { a := a + 1 }\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #13', async () => {
  const ast = parser.parse('a := 0\nwhile ((){(){a < 10}()}()) { a := a + 1 }\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #14', async () => {
  const ast = parser.parse('a := 0\nwhile a < 10 | a < 30 { a := a + 1 }\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(30)
})

test('interpret statement while #15', async () => {
  const ast = parser.parse('a := 0\nwhile a < 10 & a < 30 { a := a + 1 }\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #16', async () => {
  const ast = parser.parse('a := 0\nwhile ((a < 10)) { a := a + 1 }\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret statement while #17', async () => {
  const ast = parser.parse('x:=0\ni:=0\nwhile i < 10 { j:=0\nwhile j < 10 { j:=j+1\nx:=x+1 }\ni:=i+1 }\nx')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(100)
})

test('interpret if', async () => {
  const ast = parser.parse('a:=1\nif true { a:=2 }\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if else', async () => {
  const ast = parser.parse('a:=1\nif true {a:=2} else {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if else #2', async () => {
  const ast = parser.parse('a:=1\nif false {a:=2} else {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if else #3', async () => {
  const ast = parser.parse('x:=0\na:=1\nb:=2\nif true {x:=a} else {x:=b}\nx')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if else #4', async () => {
  const ast = parser.parse('a:=1\nif 2 >= 1 {a:=2} else {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if else #5', async () => {
  const ast = parser.parse('a:=0\nif true {a:=1}\nif false {a:=2} else {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if else #6', async () => {
  const ast = parser.parse('a:=0\nif a < 1 {a:=1}\nif a >= 1 {a:=2} else {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if else #7', async () => {
  const ast = parser.parse('a := 0\nb := 0\nif a < 10 { b := b + 1\na := a + 2 }\nb+a')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if else #8', async () => {
  const ast = parser.parse('a:=0\nif ((){true}()) {a:=1}\nelse {a:=2}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if else #9', async () => {
  const ast = parser.parse('a:=0\nif ((x){1=1}(1)) {a:=1}\nelse {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if else #10', async () => {
  const ast = parser.parse('a:=0\nif (1<=1) {a:=1}\nelse {a:=2}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if elseif', async () => {
  const ast = parser.parse('a:=0\nif (1<=1) {a:=1}\nelse if (a=1) {a:=2}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if elseif #2', async () => {
  const ast = parser.parse('a:=0\nif (a<0) {a:=1}\nelse if (a>1) {a:=2}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0)
})

test('interpret if elseif #3', async () => {
  const ast = parser.parse('a:=0\nif (a<0) {a:=1}\nelse if (a<1) {a:=2}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if elseif #4', async () => {
  const ast = parser.parse('a:=0\nif (a<0) {a:=1} else if (a<1) {a:=2}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if elseif else', async () => {
  const ast = parser.parse('a:=0\nif (a>1) {a:=1}\nelse if (a>0) {a:=2}\nelse {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if elseif else #2', async () => {
  const ast = parser.parse('a:=0\nif (a>=0) {a:=1}\nelse if (a>0) {a:=2}\nelse {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret if elseif else #3', async () => {
  const ast = parser.parse('a:=0\nif (a>0) {a:=1}\nelse if (a>=0) {a:=2}\nelse {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret if elseif else #4', async () => {
  const ast = parser.parse('a:=0\nif (a>1) {a:=1} else if (a>0) {a:=2} else {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if elseif else #5', async () => {
  const ast = parser.parse('a:=0\nif (a>1) {a:=1} else if (a>0) {a:=2} else if (a>=0) {a:=3} else {a:=4}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret if elseif else #6', async () => {
  const ast = parser.parse('a:=0\nif (a>2) {a:=1} else if (a>1) {a:=2} else if (a>0) {a:=3} else {a:=4}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(4)
})

test('interpret function recursion', async () => {
  const ast = parser.parse('f := (){ a:=a+1\nif a < 10 { f() }}\na:=1\nf()\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret function double call', async () => {
  const ast = parser.parse('inc:=(){a:=a+1}\nf:=(){while a < 10 { inc() }}\na:=1\nf()\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret function double call #2', async () => {
  const ast = parser.parse('inc:=(){a:=a+1}\nf:=(){while a < 10 { if a < 22 {inc()} }}\na:=1\nf()\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(10)
})

test('interpret object self', async () => {
  const ast = parser.parse('a:=0\no:={a:1,f:(){$.a:=2\na:=3}}\no.f()\no.a')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret object self #2', async () => {
  const ast = parser.parse('a:=0\no:={a:1,f:(){$.a:=2\na:=3}}\no.f()\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret object self #3', async () => {
  const ast = parser.parse('o:={a:1,f:(a){\na:=a+1\n$.a:=a\na:=5}}\no.f(2)\no.a')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret string concat', async () => {
  const ast = parser.parse('"" + ""')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret string concat #2', async () => {
  const ast = parser.parse('"a" + ""')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('a')
})

test('interpret string concat #3', async () => {
  const ast = parser.parse('"a" + "b"')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('ab')
})

test('interpret string concat #4', async () => {
  const ast = parser.parse('"a" + "bc" + "" + "d"')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('abcd')
})

test('interpret string concat across types', async () => {
  const ast = parser.parse('"a" + 1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('a1')
})

test('interpret string concat across types #2', async () => {
  const ast = parser.parse('"a" + 1 + 2')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('a12')
})

test('interpret string concat across types #3', async () => {
  const ast = parser.parse('"a" + 1 + 2 + "b"')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('a12b')
})

test('interpret string concat across types #4', async () => {
  const ast = parser.parse('"a" + 1 + true + "b"')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('a1trueb')
})

test('interpret string concat across types #5', async () => {
  const ast = parser.parse('"ab" + 123')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('ab123')
})

test('interpret string concat across types #6', async () => {
  const ast = parser.parse('"" + 123')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('123')
})

test('interpret array concat', async () => {
  const ast = parser.parse('[] + []')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(0)
})

test('interpret array concat #2', async () => {
  const ast = parser.parse('[1] + []')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(1)
  expect(result.value[0].value).toBe(1)
})

test('interpret array concat #3', async () => {
  const ast = parser.parse('[1] + [2]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(2)
  expect(result.value[0].value).toBe(1)
  expect(result.value[1].value).toBe(2)
})

test('interpret array concat #4', async () => {
  const ast = parser.parse('[1] + [2,3] + [] + [4]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(4)
  expect(result.value[0].value).toBe(1)
  expect(result.value[1].value).toBe(2)
  expect(result.value[2].value).toBe(3)
  expect(result.value[3].value).toBe(4)
})

test('interpret array size', async () => {
  const ast = parser.parse('[1].size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret array size #2', async () => {
  const ast = parser.parse('[].size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0)
})

test('interpret array size #3', async () => {
  const ast = parser.parse('[1,2].size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret array size #4', async () => {
  const ast = parser.parse('[1,2].size + 1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret array size #5', async () => {
  const ast = parser.parse('[1,2].velikost')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret array size #6', async () => {
  const ast = parser.parse('([1,2] + [3]).size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret string size', async () => {
  const ast = parser.parse('"a".size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret string size #2', async () => {
  const ast = parser.parse('"".size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(0)
})

test('interpret string size #3', async () => {
  const ast = parser.parse('"ab".size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret string size #4', async () => {
  const ast = parser.parse('"ab".size + 1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret string size #5', async () => {
  const ast = parser.parse('"ab".velikost')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret string size #6', async () => {
  const ast = parser.parse('("ab" + "c").size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret string size #7', async () => {
  const ast = parser.parse('"ab".concat("c").size')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret string charAt', async () => {
  const ast = parser.parse('"ab".charAt(1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('b')
})

test('interpret string charAt #2', async () => {
  const ast = parser.parse('"ab".charAt(0)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('a')
})

test('interpret string charAt #3', async () => {
  const ast = parser.parse('"ab".charAt(10)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret string sub', async () => {
  const ast = parser.parse('"abc".sub(1, 3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('bc')
})

test('interpret string sub #2', async () => {
  const ast = parser.parse('"abc".sub(1, 1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret string sub #3', async () => {
  const ast = parser.parse('"abc".sub(1, 2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('b')
})

test('interpret string sub #4', async () => {
  const ast = parser.parse('"abc".sub(10, 2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret string sub #5', async () => {
  const ast = parser.parse('"abc".sub(1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('bc')
})

test('interpret string sub #6', async () => {
  const ast = parser.parse('"abc".sub(3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret string sub #7', async () => {
  const ast = parser.parse('"abc".sub(10)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret string sub #8', async () => {
  const ast = parser.parse('"abc".sub(-1)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret string sub #9', async () => {
  const ast = parser.parse('"abc".sub(1,10)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('STRING')
  expect(result.value).toBe('')
})

test('interpret number sum', async () => {
  const ast = parser.parse('1.sum(2, 3)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret number sum #2', async () => {
  const ast = parser.parse('1.sum(2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret number sum #3', async () => {
  const ast = parser.parse('1.sum()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret array eq', async () => {
  const ast = parser.parse('[1] = [1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array eq #2', async () => {
  const ast = parser.parse('[] = []')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array eq #3', async () => {
  const ast = parser.parse('[1,2] = [1,2]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array eq #4', async () => {
  const ast = parser.parse('[1,2] = [1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret array eq #5', async () => {
  const ast = parser.parse('[1,2] = [2,1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret array eq #6', async () => {
  const ast = parser.parse('[[1],2] = [[1],2]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array eq #7', async () => {
  const ast = parser.parse('[[1,3],2] = [[1],2]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret array ne', async () => {
  const ast = parser.parse('[1] != [2]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret array ne #2', async () => {
  const ast = parser.parse('[1] != [1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret array ne #3', async () => {
  const ast = parser.parse('[1,2] != [1]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret object eq', async () => {
  const ast = parser.parse('{} = {}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret object eq #2', async () => {
  const ast = parser.parse('{a:1} = {a:1}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret function eq', async () => { // always false
  const ast = parser.parse('(){} = (){}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret function ne', async () => { // always true
  const ast = parser.parse('(){} != (){}')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret void eq', async () => { // always false
  const ast = parser.parse('(){}() = (){}()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret void ne', async () => { // always false
  const ast = parser.parse('(){}() != (){}()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret complex eq', async () => {
  const ast = parser.parse('[1,{}] = [1,{}]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret complex eq #2', async () => {
  const ast = parser.parse('[[1,["a"],[1,2,3/2,[]]]] = [[1,["a"],[1,2,3/2,[]]]]')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

// TODO
// test('interpret redefine eq', async () => {
//   const ast = parser.parse('o:={eq:(x){true}}\no.eq({})')
//   const result = await interpret.execute(ast)
//   expect(result.type).toBe('BOOLEAN')
//   expect(result.value).toBe(true)
// })

// test('interpret redefine eq #2', async () => {
//   const ast = parser.parse('o:={id:1,eq:(x){x.id=id}}\no.eq({id:1})')
//   const result = await interpret.execute(ast)
//   expect(result.type).toBe('BOOLEAN')
//   expect(result.value).toBe(true)
// })

// test('interpret redefine eq #3', async () => {
//   const ast = parser.parse('o:={id:1,eq:(x){x.id=id}}\no.eq({id:2})')
//   const result = await interpret.execute(ast)
//   expect(result.type).toBe('BOOLEAN')
//   expect(result.value).toBe(false)
// })

test('interpret scoped variable', async () => {
  const ast = parser.parse('f:=(a){a:=1\na}\nf(2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret scoped variable #2', async () => {
  const ast = parser.parse('a:=3\nf:=(a){a:=1}\nf(2)\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret scoped variable #3', async () => {
  const ast = parser.parse('a:=3\nf:=(){a:=1}\nf()\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret scoped variable #4', async () => {
  const ast = parser.parse('a:=3\nf:=(a){a:=a}\nf(2)\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret scoped variable #5', async () => {
  const ast = parser.parse('a:=3\nf:=(a){}\nf(2)\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret scoped variable #6', async () => {
  const ast = parser.parse('b:=2\nf:=(a){a:=1}\nf(b)\nb')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret scoped variable #7', async () => {
  const ast = parser.parse('if true {a:=2} else {a:=3}\ntrue')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret scoped variable #8', async () => {
  const ast = parser.parse('a:=1\nif a=1 {a:=2} else {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret scoped variable #9', async () => {
  const ast = parser.parse('a:=1\nif a=1 { if a >= 1 {a:=2} else {a:=2.5} } else {a:=3}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret scoped variable #10', async () => {
  const ast = parser.parse('a:=1\nf:=(a){g:=(a){a:=2\na}\na:=g(10)+1}\nf(11)\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret scoped variable #11', async () => {
  const ast = parser.parse('a:=1\nwhile a=1 {a:=2}\na')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret scoped variable #12', async () => {
  const ast = parser.parse('i:=10\nwhile i=10 {a:=1\nwhile a<3 {a:=a+1}\ni:=a}\ni')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret scoped variable error', () => {
  const ast = parser.parse('f:=(){a:=1}\nf()\na')
  expect(() => interpret.execute(ast)).rejects.toThrow()  // a does not exist in the global scope
})

test('interpret scoped variable error #2', () => {
  const ast = parser.parse('f:=(a){a:=1}\nf(1)\na')
  expect(() => interpret.execute(ast)).rejects.toThrow()  // a does not exist in the global scope
})

test('interpret scoped variable error #3', () => {
  const ast = parser.parse('b:=1\nf:=(a){a:=1}\nf(b)\na')
  expect(() => interpret.execute(ast)).rejects.toThrow()  // a does not exist in the global scope
})

test('interpret scoped variable error #4', () => {
  const ast = parser.parse('if true {a:=1} else {a:=2}\na')
  expect(() => interpret.execute(ast)).rejects.toThrow()  // a does not exist in the global scope
})

test('interpret max steps', () => {
  const ast = parser.parse('i:=0\nwhile i<1000 {i:=i+1}\ni')
  const maxSteps_bak = interpret.stepper.maxSteps
  interpret.stepper.maxSteps = 1000
  try {
    expect(() => interpret.execute(ast)).rejects.toThrow()
  } finally {
    interpret.stepper.maxSteps = maxSteps_bak
  }
})

// TODO does not work in the test suite, but works in general
// test('interpret max steps ok', async () => {
//   const ast = parser.parse('i:=0\nwhile i<1000 {i:=i+1}\ni')
//   const maxSteps_bak = interpret.stepper.maxSteps
//   interpret.stepper.maxSteps = 100000
//   try {
//     const result = await interpret.execute(ast)
//     expect(result.type).toBe('NUMBER')
//     expect(result.value).toBe(1000)
//   } finally {
//     interpret.stepper.maxSteps = maxSteps_bak
//   }
// })

test('interpret max steps extrem', () => {
  const ast = parser.parse('i:=0\ni')
  const maxSteps_bak = interpret.stepper.maxSteps
  interpret.stepper.maxSteps = 2
  try {
    expect(() => interpret.execute(ast)).rejects.toThrow()
  } finally {
    interpret.stepper.maxSteps = maxSteps_bak
  }
})

test('interpret and evaluation', async () => {
  const ast = parser.parse('1 < 0 & x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret and evaluation #2', async () => {
  const ast = parser.parse('1 > 0 & 2 < 0 & x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret and evaluation #3', async () => {
  const ast = parser.parse('1 > 0 & 2 < 0 & x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret or evaluation', async () => {
  const ast = parser.parse('1 > 0 | x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret or evaluation #2', async () => {
  const ast = parser.parse('1 < 0 | 2 > 0 | x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret short circuit', async () => {
  const ast = parser.parse('true | x & false & y')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret short circuit #2', async () => {
  const ast = parser.parse('true & true | x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret short circuit #3', async () => {
  const ast = parser.parse('false & x | false')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret short circuit #4', async () => {
  const ast = parser.parse('1 > 0 | x & 1 < 0 & y')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret short circuit #5', async () => {
  const ast = parser.parse('1 > 0 & 2 > 1 | x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret short circuit #6', async () => {
  const ast = parser.parse('1 < 0 & x | 2 < 1')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret number round', async () => {
  const ast = parser.parse('1.5.round()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret number round #2', async () => {
  const ast = parser.parse('1.4.round()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret number round #3', async () => {
  const ast = parser.parse('a := 1.5\na.round()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret number round #4', async () => {
  const ast = parser.parse('a := 1.4\na.round()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret number round #5', async () => {
  const ast = parser.parse('1.round()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret number floor', async () => {
  const ast = parser.parse('1.5.floor()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret number floor #2', async () => {
  const ast = parser.parse('1.floor()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret number floor #3', async () => {
  const ast = parser.parse('a := 1.8\na.floor()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret number ceil', async () => {
  const ast = parser.parse('1.2.ceil()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret number ceil #2', async () => {
  const ast = parser.parse('1.ceil()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret number ceil #3', async () => {
  const ast = parser.parse('a := 1.2\na.ceil()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret function object operation ne', async () => {
  const ast = parser.parse('(){}.ne((){})')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret function object operation eq', async () => {
  const ast = parser.parse('(){}.eq((){})')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret function reference equality', async () => {
  const ast = parser.parse('f:=(){}\nf=f')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret function reference equality #2', async () => {
  const ast = parser.parse('f:=(){}\ng:=f\ng=f')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret function reference equality #3', async () => {
  const ast = parser.parse('f:=(){}\ng:=f\ng!=f')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(false)
})

test('interpret function reference equality #4', async () => {
  const ast = parser.parse('f:=(){}\ng:=(){}\ng!=f')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('BOOLEAN')
  expect(result.value).toBe(true)
})

test('interpret function high-order', async () => {
  const ast = parser.parse('(){(){1}}()()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret function high-order #2', async () => {
  const ast = parser.parse('f:=(){(){1}}\nf()()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret function closure', async () => {
  const ast = parser.parse('x:=1\nf:=(){(){x}}\nf()()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret function closure #2', async () => {
  const ast = parser.parse('f:=(){x:=1\n(){x}}\nf()()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret function closure #3', async () => {
  const ast = parser.parse('f:=(x){(){x}}\nf(1)()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret function closure #4', async () => {
  const ast = parser.parse('x:=1\nf:=(x){(){x}}\nf(x)()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret function closure #5', async () => {
  const ast = parser.parse('x:=1\nf:=(){x}\nx:=2\nf()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret function closure #6', async () => {
  const ast = parser.parse('x:=1\nf:=(){(){x}}\nx:=2\nf()()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret function closure #7', async () => {
  const ast = parser.parse('x:=1\nf:=(x){(){x}}\nx:=2\nf(3)()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret function closure #8', async () => {
  const ast = parser.parse('x:=1\nf:=(x){x:=2\n(){x}}\nx:=2\nf(3)()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret function closure #9', async () => {
  const ast = parser.parse('x:=1\nf:=(a){a:=a+x\n(b){b:=b+a\n(c){c+b}}}\nx:=2\nx+f(10)(20)(30)+x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(66)
})

test('interpret function closure #10', async () => {
  const ast = parser.parse('x:=1\nf:=(a){a:=a+x\ny:=100\n(b){b:=b+a\nz:=1000\n(c){c+b+a+y+x+z}}}\nx:=2\nx+f(10)(20)(30)+x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1180)
})

test('interpret function closure #11', async () => {                       // 4+  7  +7 
  const ast = parser.parse('x:=1\nf:=(){x:=x+1\nf:=(){x}\nx:=x+2\nf}\nx:=x+3\nx+f()()+x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(18)
})

test('interpret function closure #12', async () => {
  const ast = parser.parse('x:=1\nf:=(a){(){x+a()}}\ng:=f((){x})\ng()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(2)
})

test('interpret function execution order', async () => {
  const ast = parser.parse('x:=1\nf:=(){x:=x+1\nx}\nx+f()')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(3)
})

test('interpret function execution order #2', async () => {
  const ast = parser.parse('x:=1\nf:=(){x:=x+1\nx}\nx+f()+x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(5)
})

test('interpret function execution order #3', async () => {
  const ast = parser.parse('x:=1\nf:=(a){x:=x+a;x}\nf(x)+f(x)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(6)
})

test('interpret function execution order #4', async () => {
  const ast = parser.parse('x:=1\nf:=(a){x:=x+a;x}\nf(x)+f(x)+f(x)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(14)
})

test('interpret function execution order #5', async () => {
  const ast = parser.parse('x:=1\nf:=(a){x:=x+a;x}\nf(x)+f(x+1)+f(x+2)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(19)
})

test('interpret function execution order #6', async () => {
  const ast = parser.parse('x:=1\nf:=(a){x:=x+a;x}\nx+f(x)+f(x)+x')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(11)
})

test('interpret function nesting', async () => {
  const ast = parser.parse('o:=1\n(o){(o){o}((o){o})}(o)(o)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(1)
})

test('interpret function recursion', async () => {
  const ast = parser.parse('f:=(a){if a > 0 { a := a + f(a - 1)}\na}\nf(5)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(15)
})

test('interpret function recursion #2', async () => {
  const ast = parser.parse('f:=(a){if a > 0 { a := a + f(a - 1) + f(a - 2)}\na}\nf(5)')
  const result = await interpret.execute(ast)
  expect(result.type).toBe('NUMBER')
  expect(result.value).toBe(21)
})
