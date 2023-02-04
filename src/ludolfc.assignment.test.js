const { LudolfC } = require('./ludolfc')
const ludolfC = new LudolfC()

test('assignment number simplest', () => {
  ludolfC.execute('a := 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment number simplest #2', () => {
  ludolfC.execute('a := 256')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(256)
})

test('assignment number simplest #3', () => {
  ludolfC.execute('a := 1.2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1.2)
})

test('assignment number simplest #4', () => {
  ludolfC.execute('a := 256.12')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(256.12)
})

test('assignment number simplest underscore varname', () => {
  ludolfC.execute('_ := 256')
  expect(ludolfC.hasVariable('_')).toBe(true)
  expect(ludolfC.getVariable('_').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_').value).toBe(256)
})

test('assignment number simplest national chars', () => {
  ludolfC.execute('ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑß := 256')
  expect(ludolfC.hasVariable('ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑß')).toBe(true)
  expect(ludolfC.getVariable('ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑß').type).toBe('NUMBER')
  expect(ludolfC.getVariable('ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑß').value).toBe(256)
})

test('assignment number simplest national chars #2', () => {
  ludolfC.execute('ěščřžýáíéúůüöäñ_1ĚŠČŘŽÝÁÍÉÚŮÜÖÄÑß := 256')
  expect(ludolfC.hasVariable('ěščřžýáíéúůüöäñ_1ĚŠČŘŽÝÁÍÉÚŮÜÖÄÑß')).toBe(true)
  expect(ludolfC.getVariable('ěščřžýáíéúůüöäñ_1ĚŠČŘŽÝÁÍÉÚŮÜÖÄÑß').type).toBe('NUMBER')
  expect(ludolfC.getVariable('ěščřžýáíéúůüöäñ_1ĚŠČŘŽÝÁÍÉÚŮÜÖÄÑß').value).toBe(256)
})

test('assignment number simplest national chars #4', () => {
  ludolfC.execute('Ř := 256')
  expect(ludolfC.hasVariable('Ř')).toBe(true)
  expect(ludolfC.getVariable('Ř').type).toBe('NUMBER')
  expect(ludolfC.getVariable('Ř').value).toBe(256)
})

test('assignment number simplest float', () => {
  ludolfC.execute('a := 25.16')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(25.16)
})

test('assignment number simplest no spaces', () => {
  ludolfC.execute('a:=1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment number simplest no spaces #2', () => {
  ludolfC.execute('a:=123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(123)
})

test('assignment number simplest space chars', () => {
  ludolfC.execute('\t\n\n \na\t \t := \t  1\t\t\n\n\n')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment number simplest loger varname', () => {
  ludolfC.execute('abc := 1')
  expect(ludolfC.hasVariable('abc')).toBe(true)
  expect(ludolfC.getVariable('abc').type).toBe('NUMBER')
  expect(ludolfC.getVariable('abc').value).toBe(1)
})

test('assignment number simplest complex varname', () => {
  ludolfC.execute('__1abc_1__xy_QWERTY_ := 123')
  expect(ludolfC.hasVariable('__1abc_1__xy_QWERTY_')).toBe(true)
  expect(ludolfC.getVariable('__1abc_1__xy_QWERTY_').type).toBe('NUMBER')
  expect(ludolfC.getVariable('__1abc_1__xy_QWERTY_').value).toBe(123)
})

test('assignment string simplest', () => {
  ludolfC.execute('a := "abc"')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('abc')
})

test('assignment string empty', () => {
  ludolfC.execute('a := ""')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('')
})

test('assignment string different quotation', () => {
  ludolfC.execute(`a := 'abc'`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('abc')
})

test('assignment string different quotation #2', () => {
  ludolfC.execute(`a := “abc”`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('abc')
})

test('assignment string different quotation #3', () => {
  ludolfC.execute(`a := ”abc”`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('abc')
})

test('assignment string different quotation #4', () => {
  ludolfC.execute(`a := “abc“`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('abc')
})

test('assignment string spaces', () => {
  ludolfC.execute(`a := "   a b  c    "`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('   a b  c    ')
})

test('assignment string spaces #2', () => {
  ludolfC.execute(`a := ("   a b  c    ")`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('   a b  c    ')
})

test('assignment string spaces #3', () => {
  ludolfC.execute(`a := ("   a b  c \t\n  \n  ")`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('   a b  c \t\n  \n  ')
})

test('assignment string tabs', () => {
  ludolfC.execute(`a := "\t   \t\t a b  c    \t"`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('\t   \t\t a b  c    \t')
})

test('assignment string newlines', () => {
  ludolfC.execute(`a := "\n\n\t   \t\t \n a b  c  \n\n  \t\n"`)
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('\n\n\t   \t\t \n a b  c  \n\n  \t\n')
})

test('assignment boolean true', () => {
  ludolfC.execute('b := true')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('b').value).toBe(true)
})

test('assignment boolean true upper', () => {
  ludolfC.execute('b := tRUe')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('b').value).toBe(true)
})

test('assignment boolean false', () => {
  ludolfC.execute('b := false')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('b').value).toBe(false)
})

test('assignment boolean false upper', () => {
  ludolfC.execute('b := FalsE')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('b').value).toBe(false)
})

test('assignment string true', () => {
  ludolfC.execute('b := "true"')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('STRING')
  expect(ludolfC.getVariable('b').value).toBe('true')
})

test('assignment string false', () => {
  ludolfC.execute('b := "false"')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('STRING')
  expect(ludolfC.getVariable('b').value).toBe('false')
})

test('assignment boolean true localized', () => {
  ludolfC.execute('b := pravda')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('b').value).toBe(true)
})

test('assignment boolean true upper localized', () => {
  ludolfC.execute('b := PravdA')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('b').value).toBe(true)
})

test('assignment boolean false localized', () => {
  ludolfC.execute('b := nepravda')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('b').value).toBe(false)
})

test('assignment boolean false upper localized', () => {
  ludolfC.execute('b := NEpravda')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('b').value).toBe(false)
})

test('assignment two statements', () => {
  ludolfC.execute('a := 123\nb := "false"')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(123)
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('STRING')
  expect(ludolfC.getVariable('b').value).toBe('false')
})

test('assignment three statements', () => {
  ludolfC.execute('a := 123\n\nb := "false"\n\nc := TRUE')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(123)
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('STRING')
  expect(ludolfC.getVariable('b').value).toBe('false')
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('c').value).toBe(true)
})

test('assignment simple var reference', () => {
  ludolfC.execute('a := 123\nb := a')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(123)
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(123)
})

test('assignment simple var self reference', () => {
  ludolfC.execute('a := 1\na := a')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})


test('assignment complex name var reference', () => {
  ludolfC.execute('a_1 := 123\na_2 := a_1')
  expect(ludolfC.hasVariable('a_1')).toBe(true)
  expect(ludolfC.getVariable('a_1').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a_1').value).toBe(123)
  expect(ludolfC.hasVariable('a_2')).toBe(true)
  expect(ludolfC.getVariable('a_2').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a_2').value).toBe(123)
})

test('assignment complex name var reference #2', () => {
  ludolfC.execute('_1 := 123\n_2 := _1')
  expect(ludolfC.hasVariable('_1')).toBe(true)
  expect(ludolfC.getVariable('_1').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_1').value).toBe(123)
  expect(ludolfC.hasVariable('_2')).toBe(true)
  expect(ludolfC.getVariable('_2').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_2').value).toBe(123)
})

test('assignment error wrong var name', () => {
  expect(() => ludolfC.execute('1 := 2')).toThrow()
})

test('assignment error wrong var name #2', () => {
  expect(() => ludolfC.execute('1a := 2')).toThrow()
})

test('assignment error wrong var name #3', () => {
  expect(() => ludolfC.execute('1_ := 2')).toThrow()
})

test('assignment error wrong var name #4', () => {
  expect(() => ludolfC.execute('1aa_bb := 2')).toThrow()
})

test('assignment error wrong var name #5', () => {
  expect(() => ludolfC.execute('$ := 2')).toThrow()
})

test('assignment error wrong var name #6', () => {
  expect(() => ludolfC.execute('# := 2')).toThrow()
})

test('assignment error wrong var name #7', () => {
  expect(() => ludolfC.execute('💩 := 2')).toThrow()
})

test('assignment error wrong value', () => {
  expect(() => ludolfC.execute('a := 1b')).toThrow()
})

test('assignment error wrong value #2', () => {
  expect(() => ludolfC.execute('a := 1 2')).toThrow()
})

test('assignment error var wrong reference', () => {
  expect(() => ludolfC.execute('a := b')).toThrow()
})

test('assignment error var wrong reference two statements', () => {
  expect(() => ludolfC.execute('a := 1\nb := c')).toThrow()
})

test('assignment error var wrong reference three statements', () => {
  expect(() => ludolfC.execute('a := 1\nb := a\nb := c')).toThrow()
})

test('assignment error var wrong self reference', () => {
  expect(() => ludolfC.execute('a := a')).toThrow()
})

test('assignment error two vars wrong reference', () => {
  expect(() => ludolfC.execute('a := 123\nb := c')).toThrow()
})

test('assignment error unfinished', () => {
  expect(() => ludolfC.execute('a :')).toThrow()
})

test('assignment error space', () => {
  expect(() => ludolfC.execute('a : = 1')).toThrow()
})

test('assignment error space #2', () => {
  expect(() => ludolfC.execute('a :\n= 1')).toThrow()
})

test('assignment error ill', () => {
  expect(() => ludolfC.execute('a : 1')).toThrow()
})

test('assignment error ill #2', () => {
  expect(() => ludolfC.execute('a :: 1')).toThrow()
})

test('assignment error ill #3', () => {
  expect(() => ludolfC.execute('a :- 1')).toThrow()
})

test('assignment error incomplete', () => {
  expect(() => ludolfC.execute('a :=')).toThrow()
})

test('assignment error expression', () => {
  expect(() => ludolfC.execute('a + 1 := 2')).toThrow()
  expect(() => ludolfC.execute('a + a := 2')).toThrow()
  expect(() => ludolfC.execute('a := 1\na + 1 := 2')).toThrow()
  expect(() => ludolfC.execute('a := 1\na + a := 2')).toThrow()
  expect(() => ludolfC.execute('a := [1]\na[0] + 1 := 2')).toThrow()
  expect(() => ludolfC.execute('a := [1]\na[0] + a[0] := 2')).toThrow()
  expect(() => ludolfC.execute('a := [1]\na[0] + a[0] + 1 := 2')).toThrow()
  expect(() => ludolfC.execute('o := {a:1}\no.a + 1 := 2')).toThrow()
  expect(() => ludolfC.execute('o := {a:1}\no.a + o.a := 2')).toThrow()
  expect(() => ludolfC.execute('o := {a:1}\no.a + o.a + 1 := 2')).toThrow()
})

test('assignment error keywords', () => {
  expect(() => ludolfC.execute('true := 1')).toThrow()
  expect(() => ludolfC.execute('false := 1')).toThrow()
  expect(() => ludolfC.execute('if := 1')).toThrow()
  expect(() => ludolfC.execute('else := 1')).toThrow()
  expect(() => ludolfC.execute('while := 1')).toThrow()
})

test('assignment error keywords upper', () => {
  expect(() => ludolfC.execute('True := 1')).toThrow()
  expect(() => ludolfC.execute('False := 1')).toThrow()
  expect(() => ludolfC.execute('If := 1')).toThrow()
  expect(() => ludolfC.execute('Else := 1')).toThrow()
  expect(() => ludolfC.execute('While := 1')).toThrow()
})

test('assignment error keywords localized', () => {
  expect(() => ludolfC.execute('pravda := 1')).toThrow()
  expect(() => ludolfC.execute('nepravda := 1')).toThrow()
  expect(() => ludolfC.execute('pokud := 1')).toThrow()
  expect(() => ludolfC.execute('jinak := 1')).toThrow()
  expect(() => ludolfC.execute('dokud := 1')).toThrow()
})

test('assignment not keywords', () => {
  ludolfC.execute('_while := 123\nwhile_ := _while')
  expect(ludolfC.hasVariable('_while')).toBe(true)
  expect(ludolfC.getVariable('_while').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_while').value).toBe(123)
  expect(ludolfC.hasVariable('while_')).toBe(true)
  expect(ludolfC.getVariable('while_').type).toBe('NUMBER')
  expect(ludolfC.getVariable('while_').value).toBe(123)
})

test('assignment simple bi expression plus', () => {
  ludolfC.execute('a := 1 + 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment simple bi expression plus #2', () => {
  ludolfC.execute('a := 1.5 + 2.5')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(4)
})

test('assignment simple bi expression minus', () => {
  ludolfC.execute('a := 1 - 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-1)
})

test('assignment simple bi expression minus #2', () => {
  ludolfC.execute('a := 1.5 - 2.5')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-1)
})

test('assignment simple bi expression multiplication', () => {
  ludolfC.execute('a := 2 * 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
})

test('assignment simple bi expression multiplication #2', () => {
  ludolfC.execute('a := 2.5 * 3.5')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(8.75)
})

test('assignment simple bi expression division', () => {
  ludolfC.execute('a := 6 / 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment simple bi expression division #2', () => {
  ludolfC.execute('a := 6.8 / 3.2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2.125)
})

test('assignment simple bi expression gt', () => {
  ludolfC.execute('a := 4 > 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression ge', () => {
  ludolfC.execute('a := 3 >= 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression lt', () => {
  ludolfC.execute('a := 2 < 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression le', () => {
  ludolfC.execute('a := 2 <= 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression ne', () => {
  ludolfC.execute('a := 2 != 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression ne false', () => {
  ludolfC.execute('a := 2 != 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple bi expression eq', () => {
  ludolfC.execute('a := 2 = 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression eq false', () => {
  ludolfC.execute('a := 2 = 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple bi expression and', () => {
  ludolfC.execute('a := true & true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression and false', () => {
  ludolfC.execute('a := true & false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple bi expression or', () => {
  ludolfC.execute('a := true | false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression or false', () => {
  ludolfC.execute('a := false | false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple bi expression no spaces', () => {
  ludolfC.execute('a:=2+3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(5)
})

test('assignment simple bi expression no spaces three members', () => {
  ludolfC.execute('a:=2+3+1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
})

test('assignment simple bi expression no spaces two-chars', () => {
  ludolfC.execute('a:=2!=1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple bi expression three members', () => {
  ludolfC.execute('a := 2 + 3 - 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(4)
})

test('assignment simple bi expression three members ref', () => {
  ludolfC.execute('a := 1\nb := 2 + 3 - a')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(4)
})

test('assignment simple bi expression three members ref #2', () => {
  ludolfC.execute('a := 1\nb := a + 1\nc := a + b + 3')
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(6)
})

test('assignment simple uni expression', () => {
  ludolfC.execute('a := !false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression', () => {
  ludolfC.execute('a := !false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression no spaces', () => {
  ludolfC.execute('a:=!false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression two members', () => {
  ludolfC.execute('a := !false | !true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression two members #2', () => {
  ludolfC.execute('a := !false & !true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple uni expression three members', () => {
  ludolfC.execute('a := !false & !true | !false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression three members #2', () => {
  ludolfC.execute('a := 2 > 3 | !false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression three members #3', () => {
  ludolfC.execute('a := 2 < 3 | !false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression three members #4', () => {
  ludolfC.execute('a := 2 > 3 | !true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple uni expression three members #5', () => {
  ludolfC.execute('a := 2 >= 3 | !true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple uni expression three members #6', () => {
  ludolfC.execute('a := 2 <= 3 | !true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression var three members', () => {
  ludolfC.execute('tr := true\nf := false\na := !f & !tr | !f')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple double uni expression', () => {
  ludolfC.execute('a := !!true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple double uni expression var', () => {
  ludolfC.execute('t := true\na := !!t')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple double uni expression var three members', () => {
  ludolfC.execute('tr := true\nf := false\na := !!tr & !!f | !!tr')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple triple uni expression', () => {
  ludolfC.execute('a := !!!true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple triple uni expression', () => {
  ludolfC.execute('a := !!!false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression minus', () => {
  ludolfC.execute('a := -1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-1)
})

test('assignment simple uni expression minus #2', () => {
  ludolfC.execute('a := -123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-123)
})

test('assignment simple uni expression minus #3', () => {
  ludolfC.execute('a := 123 > -123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression minus #4', () => {
  ludolfC.execute('a := -123 > -123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple uni expression minus #4', () => {
  ludolfC.execute('a := -123 >= -123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression minus #5', () => {
  ludolfC.execute('a:=-123>=-123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression minus #6', () => {
  ludolfC.execute('a := --123 >= --123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression minus #7', () => {
  ludolfC.execute('a := --123 >= ---123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple uni expression minus #8', () => {
  ludolfC.execute('a := ---123 >= --123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment simple uni expression minus #9', () => {
  ludolfC.execute('a :=---123')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-123)
})

test('assignment simple type change', () => {
  ludolfC.execute('a := 1\na := "A"')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('STRING')
  expect(ludolfC.getVariable('a').value).toBe('A')
})

test('assignment simple type change #2', () => {
  ludolfC.execute('a := 1\na := 2.34')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2.34)
})

test('assignment simple type change #3', () => {
  ludolfC.execute('a := 1\na := true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple type change #4', () => {
  ludolfC.execute('a := 1\na := a >= 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment simple type change #5', () => {
  ludolfC.execute('a := 1\na := {}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
})

test('assignment simple type change #6', () => {
  ludolfC.execute('a := 1\na := []')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
})

test('assignment simple type change #7', () => {
  ludolfC.execute('a := {}\na := 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment simple type change #8', () => {
  ludolfC.execute('a := []\na := 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment op precedence', () => {
  ludolfC.execute('a := 3 / 9 * 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment op precedence #2', () => {
  ludolfC.execute('a := 1 + 2 * 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(7)
})

test('assignment op precedence #3', () => {
  ludolfC.execute('a := 1 * 2 + 3')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(5)
})

test('assignment op precedence #4', () => {
  ludolfC.execute('a := 1 + 6 / 3 + 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(5)
})

test('assignment op precedence #5', () => {
  ludolfC.execute('a := 1 + 6 % 3 + 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment op precedence #6', () => {
  ludolfC.execute('a := 1 + 6 % 3 + 2 > 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment op precedence #7', () => {
  ludolfC.execute('a := 1 + 6 % 3 + 2 > 1 != false')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment op precedence #8', () => {
  ludolfC.execute('a := 1 + 6 % 3 + 2 > 1 != false & 2 <= 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment op precedence #9', () => {
  ludolfC.execute('a := 1 + 6 % 3 + 2 > 1 != false & 1 <= 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment op precedence #10', () => {
  ludolfC.execute('a := 1 + 6 % 3 + 2 > 1 != false & 123 <= 1 | 12 + 11 * -1 = 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment op precedence #11', () => {
  ludolfC.execute('a := 1 + 6 % 3 + 2 > 1 != !false & 123 <= 1 | 12 + 11 * -1 >= 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment op precedence #12', () => {
  ludolfC.execute('x := 2\na := 1 + 6 % 3 + x > 1 != !false & 123 <= 1 | 12 + 11 * -1 >= x')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment op precedence #13', () => {
  ludolfC.execute('_1 := 1\n_2 := 2\na := _1 + 6 % 3 + _2 > _1 != false & 123 <= _1 | 12 + 11 * -_1 = _1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment op precedence #14', () => {
  ludolfC.execute('_1 := 1\n_2 := 2\na := _1 + 6 % 3 + _2 > _1 != !!false & 123 <= _1 | 12 + 11 * ---_1 = _1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment op precedence #15', () => {
  ludolfC.execute('a := 1 + 2 + 3 * 3 - 2 - 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(9)
})

test('assignment op precedence #16', () => {
  ludolfC.execute('a := 1 + 2 + 3 * 3 / 3 * 3 - 2 - 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(9)
})

test('assignment op precedence #17', () => {
  ludolfC.execute('a := --1 + 1 - -1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment op precedence #18', () => {
  ludolfC.execute('a := -1 - --1 * -1 * -1 - -1 - --1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-2)
})

test('assignment grouping', () => {
  ludolfC.execute('a := (1)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment grouping #2', () => {
  ludolfC.execute('a := ((1))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment grouping #3', () => {
  ludolfC.execute('a := (((123)))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(123)
})

test('assignment grouping precedence', () => {
  ludolfC.execute('a := (1 + 6) / (3 + 2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1.4)
})

test('assignment grouping precedence #2', () => {
  ludolfC.execute('a := ((1 + 6)) / (3 + 2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1.4)
})

test('assignment grouping precedence #3', () => {
  ludolfC.execute('a := (((1 + 6)) / (3 + 2))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1.4)
})

test('assignment grouping precedence #4', () => {
  ludolfC.execute('a := (1 + (2) + 3)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
})

test('assignment grouping precedence #5', () => {
  ludolfC.execute('_1 := 1\na := (((_1) + 3) / ((3) + _1))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment grouping precedence #6', () => {
  ludolfC.execute('_1 := (1) + 0\na := (((_1) + 3) / ((3) + _1))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment grouping precedence #7', () => {
  ludolfC.execute('_1 := 1\n_2 := 2\na := ((((((((_1 + 6)) % ((3) + _2)) > _1) != !((!false))) & (123.23 <= _1))) | ((12 + 10) * ---_1 = _1 * -22))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment grouping precedence #8', () => {
  ludolfC.execute('a := (1.5 + (2.6) + 3.7)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(7.8)
})

test('assignment error grouping', () => {
  expect(() => ludolfC.execute('a := ((1)')).toThrow()
})

test('assignment error grouping #2', () => {
  expect(() => ludolfC.execute('a := (1))')).toThrow()
})

test('assignment error grouping #3', () => {
  expect(() => ludolfC.execute('a := (1')).toThrow()
})

test('assignment error grouping #4', () => {
  expect(() => ludolfC.execute('a := 1)')).toThrow()
})

test('assignment error grouping #5', () => {
  expect(() => ludolfC.execute('a := (1+)(2)')).toThrow()
})

test('assignment error grouping #6', () => {
  expect(() => ludolfC.execute('a := (1 + (2 + 3)')).toThrow()
})

test('assignment error grouping #7', () => {
  expect(() => ludolfC.execute('a := ()')).toThrow()
})

test('assignment error grouping #8', () => {
  expect(() => ludolfC.execute('a := (())')).toThrow()
})

test('assignment func simple', () => {
  ludolfC.execute('a := 1.plus(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment func simple #2', () => {
  ludolfC.execute('a := 1.plus(2) + 4')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(7)
})

test('assignment func simple #3', () => {
  ludolfC.execute('a := 5 + 1.plus(2) + 4')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(12)
})

test('assignment func simple #4', () => {
  ludolfC.execute('a := !true.neg()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #5', () => {
  ludolfC.execute('a := !!true.neg()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #6', () => {
  ludolfC.execute('a := !1.gt(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #7', () => {
  ludolfC.execute('a := !!1.gt(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #8', () => {
  ludolfC.execute('a := !!1.gt(2) | true')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #9', () => {
  ludolfC.execute('a := false & !1.gt(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #10', () => {
  ludolfC.execute('a := 123.plus(23) + 12.minus(11) * 12.minus(10)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(148)
})

test('assignment func simple #11', () => {
  ludolfC.execute('a := 123.plus(12.minus(10) * 12.minus(9))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(129)
})

test('assignment func simple #12', () => {
  ludolfC.execute('a := 123.plus(10).plus(55)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(188)
})

test('assignment func simple #13', () => {
  ludolfC.execute('a := 123.plus(10).plus(55).minus(88) + 123.plus(10).plus(55).minus(88)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(200)
})

test('assignment func simple #14', () => {
  ludolfC.execute('a := 123.plus(10).plus(55).minus(88).plus(123.plus(10).plus(55).minus(88))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(200)
})

test('assignment func simple #15', () => {
  ludolfC.execute('a := 1 - 123.plus(10).plus(55).minus(88).plus(123.plus(10).plus(55).minus(88)) + 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-197)
})

test('assignment func simple #15 spaces', () => {
  ludolfC.execute('a := 1 - 123 . plus ( 10 ) . plus ( 55 ) . minus ( 88 ) . plus ( 123 . plus ( 10 ) . plus ( 55 ) . minus(88) ) + 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-197)
})

test('assignment func simple #15 vars', () => {
  ludolfC.execute('_123:=123\n_10:=10\na := 1 - _123 . plus ( _10 ) . plus ( 55 ) . minus ( 88 ) . plus ( _123 . plus ( _10 ) . plus ( 55 ) . minus(88) ) + 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(-197)
})

test('assignment func simple #16', () => {
  ludolfC.execute('a := 123.plus(10.plus(55))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(188)
})

test('assignment func simple #16 grouping', () => {
  ludolfC.execute('a := 123.plus((((10.plus(55)))))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(188)
})

test('assignment func simple #16 grouping #2', () => {
  ludolfC.execute('a := 123.plus((((10.plus(((50 + 5)))))))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(188)
})

test('assignment func simple #16 grouping #3', () => {
  ludolfC.execute('a := (123.plus((((10.plus(((50 + 5))))))) - 8) + 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(181)
})

test('assignment func simple #17', () => {
  ludolfC.execute('a := 5.mod(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment func simple #18', () => {
  ludolfC.execute('a := 5.mult(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(10)
})

test('assignment func simple #19', () => {
  ludolfC.execute('a := 5.div(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2.5)
})

test('assignment func simple #20', () => {
  ludolfC.execute('a := 2.lt(5)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #21', () => {
  ludolfC.execute('a := 5.lt(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #22', () => {
  ludolfC.execute('a := 2.gt(5)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #23', () => {
  ludolfC.execute('a := 5.gt(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #24', () => {
  ludolfC.execute('a := 5.ge(2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #25', () => {
  ludolfC.execute('a := 5.ge(5)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #26', () => {
  ludolfC.execute('a := 5.ge(4)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #27', () => {
  ludolfC.execute('a := 4.le(4)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #28', () => {
  ludolfC.execute('a := 4.le(3)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #29', () => {
  ludolfC.execute('a := 4.le(5)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #30', () => {
  ludolfC.execute('a := 4.eq(5)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #31', () => {
  ludolfC.execute('a := 5.eq(5)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #32', () => {
  ludolfC.execute('a := 5.eq(4)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #33', () => {
  ludolfC.execute('a := 5.ne(4)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #34', () => {
  ludolfC.execute('a := 4.ne(5)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #35', () => {
  ludolfC.execute('a := 5.ne(5)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #36', () => {
  ludolfC.execute('a := true.and(true)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #37', () => {
  ludolfC.execute('a := true.and(false)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #38', () => {
  ludolfC.execute('a := false.and(false)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #39', () => {
  ludolfC.execute('a := false.or(false)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #40', () => {
  ludolfC.execute('a := false.or(true)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #41', () => {
  ludolfC.execute('a := false.xor(true)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #42', () => {
  ludolfC.execute('a := false.xor(false)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #43', () => {
  ludolfC.execute('a := false.nand(false)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #44', () => {
  ludolfC.execute('a := false.nand(true)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #45', () => {
  ludolfC.execute('a := true.nand(true)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #46', () => {
  ludolfC.execute('a := true.neg()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(false)
})

test('assignment func simple #47', () => {
  ludolfC.execute('a := false.neg()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value).toBe(true)
})

test('assignment func simple #48', () => {
  ludolfC.execute('a := 123.plus(((12)))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(135)
})

test('assignment func multiple params', () => {
  ludolfC.execute('a := (123.plus((((10.plus(((50 + 5))))))) - 8) + 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(181)
})

test('assignment func double params', () => {
  ludolfC.execute('a := 1.sum(2,3)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
})

test('assignment func double params spaces', () => {
  ludolfC.execute('a := 1.sum(  2   ,    3    )')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
})

test('assignment func double params #2', () => {
  ludolfC.execute('a := 1.sum((2), (3))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
})

test('assignment func double params #3', () => {
  ludolfC.execute('a := 1.sum((2 - 1), (3 + 1))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
})

test('assignment func double params #4', () => {
  ludolfC.execute('a := 1.sum(2.minus(1), (3.plus(1))).minus(1) + 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
})

test('assignment func triple params', () => {
  ludolfC.execute('a := 123.sum(12,2,55) + 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(193)
})

test('assignment func triple params #2', () => {
  ludolfC.execute('a := 1.sum(2.minus(1), (3.plus(1)), 123).minus(1) + 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(129)
})

test('assignment func triple params var', () => {
  ludolfC.execute('x := 123\na := x.sum(2.minus(1), (3.plus(1)), x).minus(1) + 1')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(251)
})

test('assignment error func multi params', () => {
  expect(() => ludolfC.execute('a := 1.sum((2,3))')).toThrow()
})

test('assignment error func multi params #2', () => {
  expect(() => ludolfC.execute('a := 1.sum((2,3)')).toThrow()
})

test('assignment error func multi params #3', () => {
  expect(() => ludolfC.execute('a := 1.sum((2, 3)')).toThrow()
})

test('assignment error func multi params #4', () => {
  expect(() => ludolfC.execute('a := 1.sum((123, (2), 3))')).toThrow()
})

test('assignment error func multi params #5', () => {
  expect(() => ludolfC.execute('a := 1.sum(123, (2, 3))')).toThrow()
})

test('assignment error func multi params #6', () => {
  expect(() => ludolfC.execute('a := 1.sum((123, 2), 3)')).toThrow()
})

test('assignment error func not exists', () => {
  expect(() => ludolfC.execute('a := 1.xxx()')).toThrow()
})

test('assignment error func not exists #2', () => {
  expect(() => ludolfC.execute('a := 1.xxx(123)')).toThrow()
})

test('assignment error func not exists #3', () => {
  expect(() => ludolfC.execute('a := 1.xxx(12,3)')).toThrow()
})

test('assignment error attribute not exists', () => {
  expect(() => ludolfC.execute('a := 1.xxx')).toThrow()
})

test('assignment error attribute not exists #2', () => {
  expect(() => ludolfC.execute('a := 1\nb := a.xxx')).toThrow()
})

test('assignment array definition empty', () => {
  ludolfC.execute('a := []')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toEqual(expect.arrayContaining([]))
})

test('assignment array definition one dimension', () => {
  ludolfC.execute('a := [1]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(1)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(1)
})

test('assignment array definition one dimension #2', () => {
  ludolfC.execute('a := [1,2,3]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(2)
  expect(ludolfC.getVariable('a').value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[2].value).toBe(3)
})

test('assignment array definition one dimension #2 spaces', () => {
  ludolfC.execute('a := [  1 ,2  ,   3   ]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(2)
  expect(ludolfC.getVariable('a').value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[2].value).toBe(3)
})

test('assignment array definition one dimension #2 newlines', () => {
  ludolfC.execute('a := [\n1\n,\n2]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(2)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(2)
})

test('assignment array definition one dimension #2 newlines #2', () => {
  ludolfC.execute('a := [\n1\n,\n2\n,\n3\n]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(2)
  expect(ludolfC.getVariable('a').value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[2].value).toBe(3)
})

test('assignment array definition one dimension #2 newlines and space', () => {
  ludolfC.execute('a := [ \n  1\n ,\n2  \n,\n   3   \n ]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(2)
  expect(ludolfC.getVariable('a').value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[2].value).toBe(3)
})

test('assignment array definition one dimension #3', () => {
  ludolfC.execute('a := [0.plus(1) + 1, 3.minus(1) +12, 10 + 2.plus(1)]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(2)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(14)
  expect(ludolfC.getVariable('a').value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[2].value).toBe(13)
})

test('assignment array definition one dimension #4', () => {
  ludolfC.execute('x := 1\na := [(0.plus(1) + 1), 3.minus(x) +12, 10 + x.plus(x +x)]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(2)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(14)
  expect(ludolfC.getVariable('a').value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[2].value).toBe(13)
})

test('assignment array definition one dimension #5', () => {
  ludolfC.execute('x := 1\na := ([(0.plus(1) + 1), 3.minus(x) +12, 10 + x.plus(x +x)])')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(2)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(14)
  expect(ludolfC.getVariable('a').value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[2].value).toBe(13)
})

test('assignment array definition one dimension heterogen', () => {
  ludolfC.execute('a := [123, "123", 123 > 100]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(123)
  expect(ludolfC.getVariable('a').value[1].type).toBe('STRING')
  expect(ludolfC.getVariable('a').value[1].value).toBe('123')
  expect(ludolfC.getVariable('a').value[2].type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value[2].value).toBe(true)
})

test('assignment array definition one dimension concat', () => {
  ludolfC.execute('a := [1,2].concat([3,4])')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(4)
  expect(ludolfC.getVariable('a').value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value).toBe(2)
  expect(ludolfC.getVariable('a').value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[2].value).toBe(3)
  expect(ludolfC.getVariable('a').value[3].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[3].value).toBe(4)
})

test('assignment array definition two dimensions', () => {
  ludolfC.execute('a := [[1,2],[3]]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(2)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(2)
  expect(ludolfC.getVariable('a').value[0].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[0].value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[1].value).toBe(2)
  
  expect(ludolfC.getVariable('a').value[1].value).toHaveLength(1)
  expect(ludolfC.getVariable('a').value[1].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value[0].value).toBe(3)
})

test('assignment array definition two dimensions spaces', () => {
  ludolfC.execute('  a := [  [  1  ,  2    ]   ,  [    3   ]   ]   ')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(2)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(2)
  expect(ludolfC.getVariable('a').value[0].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[0].value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[1].value).toBe(2)
  
  expect(ludolfC.getVariable('a').value[1].value).toHaveLength(1)
  expect(ludolfC.getVariable('a').value[1].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value[0].value).toBe(3)
})

test('assignment array definition two dimensions newlines', () => {
  ludolfC.execute('a := [\n[\n1\n]\n]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(1)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(1)
  expect(ludolfC.getVariable('a').value[0].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[0].value).toBe(1)
})

test('assignment array definition two dimensions spaces and newlines', () => {
  ludolfC.execute('a := [ \n [ \n 1 \n ] \n ] ')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(1)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(1)
  expect(ludolfC.getVariable('a').value[0].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[0].value).toBe(1)
})

test('assignment array definition two dimensions spaces and newlines #2', () => {
  ludolfC.execute('a := [\n[\n1\n,\n2\n\n]\n]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(1)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(2)
  expect(ludolfC.getVariable('a').value[0].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[0].value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[1].value).toBe(2)
})

test('assignment array definition two dimensions spaces and newlines #3', () => {
  ludolfC.execute('  a := [\n  [\n  1  \n, \n 2 \n  \n ] \n  ,\n\n  [\n\n    3\n  \n ] \n\n \n ] \n  ')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(2)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(2)
  expect(ludolfC.getVariable('a').value[0].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value[0].value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[0].value[1].value).toBe(2)
  
  expect(ludolfC.getVariable('a').value[1].value).toHaveLength(1)
  expect(ludolfC.getVariable('a').value[1].value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value[1].value[0].value).toBe(3)
})

test('assignment array definition two empty dimensions', () => {
  ludolfC.execute('a := [[]]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(1)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(0)
})

test('assignment array definition two empty dimensions #2', () => {
  ludolfC.execute('a := [\n[]]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(1)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(0)
})

test('assignment array definition two empty dimensions #3', () => {
  ludolfC.execute('a := [[],[]]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(2)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(0)

  expect(ludolfC.getVariable('a').value[1].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[1].value).toHaveLength(0)
})

test('assignment array definition three empty dimensions', () => {
  ludolfC.execute('a := [[[]],[]]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(2)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(1)

  expect(ludolfC.getVariable('a').value[0].value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value[0].value).toHaveLength(0)

  expect(ludolfC.getVariable('a').value[1].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[1].value).toHaveLength(0)
})

test('assignment array definition three empty dimensions spaces and newlines', () => {
  ludolfC.execute('a := [ \n \n [  \n\n [ \n \n ] \n \n ] \n \n , \n \n [ \n \n ] \n \n ] \n \n ')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(2)

  expect(ludolfC.getVariable('a').value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value).toHaveLength(1)

  expect(ludolfC.getVariable('a').value[0].value[0].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[0].value[0].value).toHaveLength(0)

  expect(ludolfC.getVariable('a').value[1].type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value[1].value).toHaveLength(0)
})

test('assignment error array definition', () => {
  expect(() => ludolfC.execute('a := [')).toThrow()
  expect(() => ludolfC.execute('a := ]')).toThrow()
  expect(() => ludolfC.execute('a := []]')).toThrow()
  expect(() => ludolfC.execute('a := [[]')).toThrow()
  expect(() => ludolfC.execute('a := [1 2]')).toThrow()
  expect(() => ludolfC.execute('a := [[][]]')).toThrow()
  expect(() => ludolfC.execute('a := [,]')).toThrow()
  expect(() => ludolfC.execute('a := [,]')).toThrow()
  expect(() => ludolfC.execute('a := [,]')).toThrow()
  expect(() => ludolfC.execute('a := [[],]')).toThrow()
  expect(() => ludolfC.execute('a := [[,]]')).toThrow()
  expect(() => ludolfC.execute('a := [[,]')).toThrow()
  expect(() => ludolfC.execute('a := [1,]')).toThrow()
  expect(() => ludolfC.execute('a := [1,2,]')).toThrow()
  expect(() => ludolfC.execute('a := \n[]')).toThrow() // definition must start on the same line
})

test('assignment array access one dimension', () => {
  ludolfC.execute('arr := [1,2,3]\na := arr[0]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment array access one dimension #2', () => {
  ludolfC.execute('arr := [1,2,3]\na := arr[2]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment array access one dimension #3', () => {
  ludolfC.execute('arr := [1,2,3]\na := arr[1 + 1]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment array access one dimension #4', () => {
  ludolfC.execute('arr := [1,2,3]\na := arr[(123.minus(100) - 22)]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment array access one dimension #4', () => {
  ludolfC.execute('arr := [1,2,3]\na := arr[(((123.minus(100) - 23)) + (1))]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment array access one dimension #5', () => {
  ludolfC.execute('arr := [1,2,3]\na := arr[1 + 1]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment array access one dimension #6', () => {
  ludolfC.execute('x:=2\ny:=33\narr := [1,x,y]\na := arr[1 + 1]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(33)
})

test('assignment error array access', () => {
  expect(() => ludolfC.execute('arr := [1,2,3]\na := arr[]')).toThrow()
  expect(() => ludolfC.execute('arr := [1,2,3]\na := arr[,]')).toThrow()
  expect(() => ludolfC.execute('arr := [1,2,3]\na := arr[1,]')).toThrow()
  expect(() => ludolfC.execute('arr := [1,2,3]\na := arr[1,2]')).toThrow()
  expect(() => ludolfC.execute('arr := [1,2,3]\na := arr[[1]]')).toThrow()
  expect(() => ludolfC.execute('arr := [1,2,3]\na := arr[3]')).toThrow()
})

test('assignment array access two dimensions', () => {
  ludolfC.execute('arr := [[1,2],[3]]\na := arr[0,1]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment array access two dimensions #2', () => {
  ludolfC.execute('arr := [[1,2],[3]]\na := arr[1,0]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment array access two dimensions #3', () => {
  ludolfC.execute('arr := [[1,2],[3]]\na := arr[1, (0.plus(1) - 1)]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment array access two dimensions #4', () => {
  ludolfC.execute('arr := [[1,2],[3,4,5]]\na := arr[1]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
})

test('assignment array access direct', () => {
  ludolfC.execute('a := [[1,2],[3]] [0,1]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment array access direct #2', () => {
  ludolfC.execute('a := [[1,2],[3]] [1, (0.plus(1) - 1)]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment array access direct #3', () => {
  ludolfC.execute('a := [[1,2],[3]][1, (0.plus(1) - 1)]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment array access direct #4', () => {
  ludolfC.execute('a := [[1,2],[3,4,5]] [1]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value).toHaveLength(3)
})

test('assignment array access direct #5', () => {
  ludolfC.execute('x:=0\ny:=1\na := [[1,2],[3,4,5]][x,y]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment array access direct #6', () => {
  ludolfC.execute('x:=0\ny:=1\na := [[y,y+1],[y+1+1,y+1+1+1,y+4+x]][x,y]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment array access direct #7', () => {
  ludolfC.execute('x:=0\ny:=1\na := [ \n  \n [\n 1 \n , \n 2 \n ] \n , \n [ \n 3 \n , \n 4 \n , \n 5 \n ] \n ]     [ \n x \n , \n y \n ] \n ')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment object definition empty', () => {
  ludolfC.execute('a := {}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value)).toHaveLength(0)
})
test('assignment object definition empty quoting', () => {
  ludolfC.execute('a := ({})')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value)).toHaveLength(0)
})

test('assignment object definition empty spaces', () => {
  ludolfC.execute('a := {   }')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value)).toHaveLength(0)
})

test('assignment object definition empty spaces and whitelines', () => {
  ludolfC.execute('a := { \n\n   }')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value)).toHaveLength(0)
})

test('assignment object definition one simple attribute', () => {
  ludolfC.execute('a := {x:1}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.x.value).toBe(1)
})

test('assignment object definition one simple attribute quoting', () => {
  ludolfC.execute('a := ({x:1})')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.x.value).toBe(1)
})

test('assignment object definition one simple attribute #2', () => {
  ludolfC.execute('a := {_x1:1}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value._x1.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value._x1.value).toBe(1)
})

test('assignment object definition one simple attribute #3', () => {
  ludolfC.execute('a := {  \n\n _x1   \n\n  :  \n\n  1  \n\n }')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value._x1.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value._x1.value).toBe(1)
})

test('assignment object definition one simple attribute #4', () => {
  ludolfC.execute('a := {x:""}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.x.value).toBe('')
})

test('assignment object definition one simple attribute #5', () => {
  ludolfC.execute('a := {x:"abc"}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.x.value).toBe('abc')
})

test('assignment object definition one simple attribute #6', () => {
  ludolfC.execute('a := {x:"abc,a:3,x:4"}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.x.value).toBe('abc,a:3,x:4')
})

test('assignment object definition two simple attributes', () => {
  ludolfC.execute('a := {x:1, y:2}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.x.value).toBe(1)
  expect(ludolfC.getVariable('a').value.y.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.y.value).toBe(2)
})

test('assignment object definition two simple attributes #2', () => {
  ludolfC.execute('a := {x:1, y:"abc"}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.x.value).toBe(1)
  expect(ludolfC.getVariable('a').value.y.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.y.value).toBe('abc')
})

test('assignment object definition two simple attributes #3', () => {
  ludolfC.execute('a := {x:123, y:"abc,a:3"}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.x.value).toBe(123)
  expect(ludolfC.getVariable('a').value.y.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.y.value).toBe('abc,a:3')
})

test('assignment object definition two simple attributes #4', () => {
  ludolfC.execute('_10:=10\na := {x_1:123,y_1:"abc,a:3",zzz:(_10 + 1)}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x_1.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.x_1.value).toBe(123)
  expect(ludolfC.getVariable('a').value.y_1.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.y_1.value).toBe('abc,a:3')
  expect(ludolfC.getVariable('a').value.zzz.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.zzz.value).toBe(11)
})

test('assignment object definition two simple attributes #5', () => {
  ludolfC.execute('_10:=10\na := {x_1:123,y_1:"abc,a:3",zzz:(_10.plus(2) + 1 * 2)}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x_1.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.x_1.value).toBe(123)
  expect(ludolfC.getVariable('a').value.y_1.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.y_1.value).toBe('abc,a:3')
  expect(ludolfC.getVariable('a').value.zzz.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.zzz.value).toBe(14)
})

test('assignment object definition two complex attributes', () => {
  ludolfC.execute('a := {o:{}}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.o.value)).toHaveLength(0)
})

test('assignment object definition two complex attributes #2', () => {
  ludolfC.execute('a := {o:{p:1}}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o.type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o.value.p.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o.value.p.value).toBe(1)
})

test('assignment object definition two complex attributes #2 spaces and newlines', () => {
  ludolfC.execute('a := {  \n\n   o \n\n  : \n\n   {   \n\n   p   \n\n   :    \n\n 1   \n\n   }   \n\n  }')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o.type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o.value.p.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o.value.p.value).toBe(1)
})

test('assignment object definition two complex attributes #3', () => {
  ludolfC.execute('a := {o1:{o2:123}}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o1.type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o1.value.o2.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o1.value.o2.value).toBe(123)
})

test('assignment object definition two complex attributes #4', () => {
  ludolfC.execute('a := {o1: {o2:123, p2: 1.plus(2) + 1 }}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o1.type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o1.value.o2.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o1.value.o2.value).toBe(123)
  expect(ludolfC.getVariable('a').value.o1.value.p2.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o1.value.p2.value).toBe(4)
})

test('assignment object definition two complex attributes #5', () => {
  ludolfC.execute('a := {o1: {o2:123, p2: 1.plus(2) + 1, q2: "" }}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o1.type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o1.value.o2.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o1.value.o2.value).toBe(123)
  expect(ludolfC.getVariable('a').value.o1.value.p2.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o1.value.p2.value).toBe(4)
  expect(ludolfC.getVariable('a').value.o1.value.q2.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.o1.value.q2.value).toBe('')
})

test('assignment object definition two complex attributes #6', () => {
  ludolfC.execute('a := {o1: {o2:123, p2: 1.plus(2) + 1, q2: "" }, oo:{oo1 : 741, pp1: "abc"}, boo: true}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o1.type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o1.value.o2.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o1.value.o2.value).toBe(123)
  expect(ludolfC.getVariable('a').value.o1.value.p2.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o1.value.p2.value).toBe(4)
  expect(ludolfC.getVariable('a').value.o1.value.q2.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.o1.value.q2.value).toBe('')
  expect(ludolfC.getVariable('a').value.oo.type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.oo.value.oo1.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.oo.value.oo1.value).toBe(741)
  expect(ludolfC.getVariable('a').value.oo.value.pp1.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.oo.value.pp1.value).toBe('abc')
  expect(ludolfC.getVariable('a').value.boo.type).toBe('BOOLEAN')
  expect(ludolfC.getVariable('a').value.boo.value).toBe(true)
})

test('assignment object definition two complex attributes #7', () => {
  ludolfC.execute('_10:=10\na := {x_1:123,y_1:"abc,a:3",zzz:(_10.plus(2) + 1 * 2)}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.x_1.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.x_1.value).toBe(123)
  expect(ludolfC.getVariable('a').value.y_1.type).toBe('STRING')
  expect(ludolfC.getVariable('a').value.y_1.value).toBe('abc,a:3')
  expect(ludolfC.getVariable('a').value.zzz.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.zzz.value).toBe(14)
})

test('assignment object definition array empty', () => {
  ludolfC.execute('a := {arr: []}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.arr.type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value.arr.value).toHaveLength(0)
  expect(ludolfC.getVariable('a').value.arr.value).toEqual(expect.arrayContaining([]))
})

test('assignment object definition array', () => {
  ludolfC.execute('a := {arr: [1]}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.arr.type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value.arr.value).toHaveLength(1)
  expect(ludolfC.getVariable('a').value.arr.value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[0].value).toBe(1)
})

test('assignment object definition array #2', () => {
  ludolfC.execute('a := {arr: [1,2,3]}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.arr.type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value.arr.value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value.arr.value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value.arr.value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[1].value).toBe(2)
  expect(ludolfC.getVariable('a').value.arr.value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[2].value).toBe(3)
})

test('assignment object definition array #3', () => {
  ludolfC.execute('a := {arr: [1,2,385]}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.arr.type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value.arr.value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value.arr.value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value.arr.value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[1].value).toBe(2)
  expect(ludolfC.getVariable('a').value.arr.value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[2].value).toBe(385)
})

test('assignment object definition array #4', () => {
  ludolfC.execute('a := {arr: [1,2, (385.plus(1))]}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.arr.type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value.arr.value).toHaveLength(3)
  expect(ludolfC.getVariable('a').value.arr.value[0].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[0].value).toBe(1)
  expect(ludolfC.getVariable('a').value.arr.value[1].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[1].value).toBe(2)
  expect(ludolfC.getVariable('a').value.arr.value[2].type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[2].value).toBe(386)
})

test('assignment object definition array object', () => {
  ludolfC.execute('a := {arr: [{}]}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.arr.type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value.arr.value[0].type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.arr.value[0].value)).toHaveLength(0)
})

test('assignment object definition array object #2', () => {
  ludolfC.execute('a := {arr:[{a:1}]}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.arr.type).toBe('ARRAY')
  expect(ludolfC.getVariable('a').value.arr.value[0].type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.arr.value[0].value.a.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.arr.value[0].value.a.value).toBe(1)
})

test('assignment object definition objects empty', () => {
  ludolfC.execute('a := {o:{o:{o:1}}}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.o.value)).toHaveLength(1)
  expect(ludolfC.getVariable('a').value.o.value.o.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.o.value.o.value)).toHaveLength(1)
  expect(ludolfC.getVariable('a').value.o.value.o.value.o.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o.value.o.value.o.value).toBe(1)
})

test('assignment object definition objects', () => {
  ludolfC.execute('a := {o:{o:{}}}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.o.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.o.value)).toHaveLength(1)
  expect(ludolfC.getVariable('a').value.o.value.o.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.o.value.o.value)).toHaveLength(0)
})

test('assignment object definition objects #2', () => {
  ludolfC.execute('a := {o:{o:1},p:{o:2}}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')

  expect(ludolfC.getVariable('a').value.o.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.o.value)).toHaveLength(1)
  expect(ludolfC.getVariable('a').value.o.value.o.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o.value.o.value).toBe(1)

  expect(ludolfC.getVariable('a').value.p.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.p.value)).toHaveLength(1)
  expect(ludolfC.getVariable('a').value.p.value.o.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.p.value.o.value).toBe(2)
})

test('assignment object definition objects #3', () => {
  ludolfC.execute('_x:=123\na := {o:{o:1},p:{o: _x.minus(120) - 1}}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')

  expect(ludolfC.getVariable('a').value.o.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.o.value)).toHaveLength(1)
  expect(ludolfC.getVariable('a').value.o.value.o.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.o.value.o.value).toBe(1)

  expect(ludolfC.getVariable('a').value.p.type).toBe('OBJECT')
  expect(Object.keys(ludolfC.getVariable('a').value.p.value)).toHaveLength(1)
  expect(ludolfC.getVariable('a').value.p.value.o.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.p.value.o.value).toBe(2)
})

test('assignment object definition objects #4', () => {
  ludolfC.execute('a := {a:1}')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('OBJECT')
  expect(ludolfC.getVariable('a').value.a.type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value.a.value).toBe(1)
})

test('assignment object definition access', () => {
  ludolfC.execute('a := {a:1}.a')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment object definition access #2', () => {
  ludolfC.execute('a := {a:1, b:2}.b')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment object definition objects #3', () => {
  ludolfC.execute('a := {o:{o:1},p:{o:2}}.o.o')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment object definition objects #4', () => {
  ludolfC.execute('a := {o:{o:1},p:{o:2}}.p.o')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment object definition objects #5', () => {
  ludolfC.execute('a := {o:[1]}.o[0]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment object definition objects #6', () => {
  ludolfC.execute('a := {o:[1]}.o[0] + {p:[2]}.p[0]')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment object definition objects #7', () => {
  ludolfC.execute('a := {o:{o:[1,2,[3,4]]}}.o.o[2,1].plus(100) * 2')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(208)
})

test('assignment error object definition', () => {
  expect(() => ludolfC.execute('a := {')).toThrow()
  expect(() => ludolfC.execute('a := }')).toThrow()
  expect(() => ludolfC.execute('a := {}}')).toThrow()
  expect(() => ludolfC.execute('a := {{}')).toThrow()
  expect(() => ludolfC.execute('a := {:}')).toThrow()
  expect(() => ludolfC.execute('a := {,}')).toThrow()
  expect(() => ludolfC.execute('a := {a:}')).toThrow()
  expect(() => ludolfC.execute('a := {:1}')).toThrow()
  expect(() => ludolfC.execute('a := {1:}')).toThrow()
  expect(() => ludolfC.execute('a := {1:2}')).toThrow()
  expect(() => ludolfC.execute('a := {}{}')).toThrow()
  expect(() => ludolfC.execute('a := {{}}')).toThrow()
  expect(() => ludolfC.execute('a := {{}:1}')).toThrow()
  expect(() => ludolfC.execute('a := {a:1}}')).toThrow()
  expect(() => ludolfC.execute('a := {{a:1}')).toThrow()
  expect(() => ludolfC.execute('a := ({a:1}')).toThrow()
  expect(() => ludolfC.execute('a := {a:1})')).toThrow()
  expect(() => ludolfC.execute('a := {(a):1}')).toThrow()
  expect(() => ludolfC.execute('a := {a:1,a:2}')).toThrow()
})

test('assignment function empty', () => {
  ludolfC.execute('f := (){}')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function empty spaces', () => {
  ludolfC.execute('f := ( ) { } ')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function empty spaces and newlines', () => {
  ludolfC.execute('f := (  \n\n  )  \n\n  {  \n\n  }  ')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function empty exec', () => {
  ludolfC.execute('f := (){}\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('VOID')
  expect(ludolfC.getVariable('a').value).toBe(null)
})

test('assignment function simple', () => {
  ludolfC.execute('f := (){1}')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function simple exec', () => {
  ludolfC.execute('f := (){1}\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment function simple #2', () => {
  ludolfC.execute('f := (){ 1 + 2 }')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function simple #2 spaces and newlines', () => {
  ludolfC.execute('f := (){\n\n  1 + 2  \n\n}')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function simple exec #2', () => {
  ludolfC.execute('f := (){ 1 + 2 }\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment function', () => {
  ludolfC.execute('f := (){\nx := 2\nx + x / 2\n}')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function exec', () => {
  ludolfC.execute('f := (){\nx := 2\nx + x / 2\n}\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment function exec #2', () => {
  ludolfC.execute('f := (){\nx := 2\ny := x + x / 2\nx + y\n}\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(5)
})

test('assignment function one arg', () => {
  ludolfC.execute('f := (x){x}')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function one arg exec', () => {
  ludolfC.execute('f := (x){x}\na := f(1)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment function one arg #2', () => {
  ludolfC.execute('f := (_x1){_x1}')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function one arg exec #2', () => {
  ludolfC.execute('f := (_x1){_x1}\na := f(1)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment function one arg #3', () => {
  ludolfC.execute('f := (x){ x + 1 }')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function one arg exec #3', () => {
  ludolfC.execute('f := (x){ x + 1 }\na := f(1)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment function one arg #4', () => {
  ludolfC.execute('f := (x){ x := x + 1\nx + 3 }')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function one arg exec #4', () => {
  ludolfC.execute('f := (x){ x := x + 1\nx + 3 }\na := f(1)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(5)
})

test('assignment function one arg exec #4 spaces and newlines', () => {
  ludolfC.execute('f := (  x  ) \n {\n   x := x + 1\nx + 3 }\na := f(1)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(5)
})

test('assignment function args', () => {
  ludolfC.execute('f := (x,y){x+y}')
  expect(ludolfC.hasVariable('f')).toBe(true)
  expect(ludolfC.getVariable('f').type).toBe('FUNCTION')
})

test('assignment function args exec', () => {
  ludolfC.execute('f := (x,y){x+y}\na := f(1,2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment function args exec #2', () => {
  ludolfC.execute('f := (x,y){_ := y + 1\ny := x + 5\n_+y}\na := f(1,2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(9)
})

test('assignment function args exec #3', () => {
  ludolfC.execute('f := (x){o := {v:x}\no.v}\na := f(1)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment function args exec #4', () => {
  ludolfC.execute('f := (x,y){o := {x:y}\ny := x\ny + o.x}\na := f(1,2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment function args exec #5', () => {
  ludolfC.execute('f := (x,y){o := { x:y, z:x }\ny := o.x + 5\no.z + y}\na := f(1,2)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(8)
})

test('assignment function args override exec', () => {
  ludolfC.execute('x := 123\nf := (x){x}\na := f(1)\nb := x')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(123)
})

test('assignment function args override exec #2', () => {
  ludolfC.execute('x := 123\nf := (x){x := x + 1\nx}\na := f(1)\nb := x')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(123)
  expect(ludolfC.hasVariable('x')).toBe(true)
  expect(ludolfC.getVariable('x').type).toBe('NUMBER')
  expect(ludolfC.getVariable('x').value).toBe(123)
})

test('assignment function args override exec #3', () => {
  ludolfC.execute('x := 1\nf := (x){y := x + 1\ng := (x){ x + y }\ng(2)}\na := f(3)')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(6)
  expect(ludolfC.hasVariable('x')).toBe(true)
  expect(ludolfC.getVariable('x').type).toBe('NUMBER')
  expect(ludolfC.getVariable('x').value).toBe(1)
})

test('assignment function args global exec', () => {
  ludolfC.execute('x := 123\nf := (){x}\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(123)
})

test('assignment function args global exec #2', () => {
  ludolfC.execute('x := 123\nf := (){g := (){x}\ng()}\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(123)
})

test('assignment function nested', () => {
  ludolfC.execute('x := 10\nf := (x){ x+1 }\ng := (x){ x*2 }\na := f(g(x))')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(21)
})

test('assignment function inner exec', () => {
  ludolfC.execute('f := (){ g := (){1}\n g() }\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment function inner exec #2', () => {
  ludolfC.execute('f := (){ f := (){1}\n f() + 1 }\na := f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment function object exec', () => {
  ludolfC.execute('o := {f:(){1}}\na := o.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment function object inner exec', () => {
  ludolfC.execute('o := {a:1,f:(){a}}\na := o.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment object inner attributes', () => {
  ludolfC.execute('o := {a:1,f:(){a}}\na := o.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(1)
})

test('assignment object inner attributes #1', () => {
  ludolfC.execute('o := { a:1, b:(){a+1} }\na := o.b()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment object inner attributes #2', () => {
  ludolfC.execute('o := { b:(){a+1}, a:1 }\na := o.b()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(2)
})

test('assignment object inner attributes #3', () => {
  ludolfC.execute('o := { a:1, b:2, f:(){a+b} }\na := o.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment object inner attributes #4', () => {
  ludolfC.execute('o := { a:1, b:2, f:(){ g:=(){a+b}\ng()} }\na := o.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment object inner attributes #5', () => {
  ludolfC.execute('o := { a:1, f:(){ b:=2\ng:=(){a+b}\ng()} }\na := o.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment object inner attributes #6', () => {
  ludolfC.execute('o := { a:1, b:{ c:2, f:(){a+c} } }\na := o.b.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment object inner attributes #7', () => {
  ludolfC.execute('o := { a:1, o:{ aa:2, f:(){a+aa} } }\na := o.o.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(3)
})

test('assignment object inner attributes #8', () => {
  ludolfC.execute('o := { a:1, o:{ a:2, f:(){a+a} } }\na := o.o.f()')
  expect(ludolfC.hasVariable('a')).toBe(true)
  expect(ludolfC.getVariable('a').type).toBe('NUMBER')
  expect(ludolfC.getVariable('a').value).toBe(4)
})

test('assignment error function definition', () => {
  expect(() => ludolfC.execute('a := !(){}')).toThrow()
  expect(() => ludolfC.execute('a := (){}.()')).toThrow()
  expect(() => ludolfC.execute('a := 1 + (){}')).toThrow()
  expect(() => ludolfC.execute('a := 1 (){}')).toThrow()
  expect(() => ludolfC.execute('a := (){} + 1')).toThrow()
  expect(() => ludolfC.execute('a := (1){}')).toThrow()
  expect(() => ludolfC.execute('a := (x,1){}')).toThrow()
  expect(() => ludolfC.execute('a := (1,2){}')).toThrow()
  expect(() => ludolfC.execute('a := (x,1,y){}')).toThrow()
})

test('assignment error function definition wrong body', () => {
  expect(() => ludolfC.execute('f := (){1a}\nf()')).toThrow()
  expect(() => ludolfC.execute('f := (){()}\nf()')).toThrow()
  expect(() => ludolfC.execute('f := (){{:}}\nf()')).toThrow()
  expect(() => ludolfC.execute('f := (){[,]}\nf()')).toThrow()
  expect(() => ludolfC.execute('f := (){ 1 2 }\nf()')).toThrow()
  expect(() => ludolfC.execute('f := (){ * 1 2 }\nf()')).toThrow()
})

test('assignment error function definition wrong call', () => {
  expect(() => ludolfC.execute('f := (x){x}\nf()')).toThrow()
  expect(() => ludolfC.execute('f := (x){x}\nf(1,2)')).toThrow()
  expect(() => ludolfC.execute('f := (x,y){x+y}\nf()')).toThrow()
  expect(() => ludolfC.execute('f := (x,y){x+y}\nf(1)')).toThrow()
  expect(() => ludolfC.execute('f := (x,y){x+y}\nf(1,2,3)')).toThrow()
  expect(() => ludolfC.execute('f := (){ g(){} \n 1}\ng()')).toThrow()
})

test('assignment array element', () => {
  ludolfC.execute('a := [1,2]\na[0] := 3\nb := a[0]\nc := a[1]')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(3)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(2)
})

test('assignment array element #2', () => {
  ludolfC.execute('a := [[1],[2,3]]\na[1][0] := 4\nb := a[1][0]\nc := a[1][1]')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(4)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(3)
})

test('assignment array element #3', () => {
  ludolfC.execute('o := {a:[[1],[2,3]]}\no.a[1][0] := 4\nb := o.a[1][0]\nc := o.a[1][1]')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(4)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(3)
})

test('assignment array element #4', () => {
  ludolfC.execute('a := [{a:[[1],[2,3]]}]\na[0].a[1][0] := 4\nb := a[0].a[1][0]\nc := a[0].a[1][1]')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(4)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(3)
})

test('assignment array element #4 spaces', () => {
  ludolfC.execute('a := [{a:[[1],[2,3]]}]\n  a  [  0  ]  .  a  [  1  ]  [  0  ]  :=  4  \nb := a[0].a[1][0]\nc := a[0].a[1][1]')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(4)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(3)
})

test('assignment array element #5', () => {
  ludolfC.execute('a := [{a:[[1],[2,3]]}]\na[0].a[1][0] := 4 + a[0].a[1][0]\nb := a[0].a[1][0]\nc := a[0].a[1][1]')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(6)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(3)
})

test('assignment array element #5 no spaces', () => {
  ludolfC.execute('a := [{a:[[1],[2,3]]}]\na[0].a[1][0]:=4+a[0].a[1][0]\nb := a[0].a[1][0]\nc := a[0].a[1][1]')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(6)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(3)
})

test('assignment error array element', () => {
  expect(() => ludolfC.execute('a := []\na[0] := 1')).toThrow()
})

test('assignment object attribute', () => {
  ludolfC.execute('a := {x:1, y:2}\na.x := 3\nb := a.x\nc := a.y')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(3)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(2)
})

test('assignment object attribute #2', () => {
  ludolfC.execute('a := [{x:1, y:2}]\na[0].x := 3\nb := a[0].x\nc := a[0].y')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(3)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(2)
})

test('assignment object attribute #3', () => {
  ludolfC.execute('a := [{a:{x:1, y:2}}]\na[0].a.x := 3\nb := a[0].a.x\nc := a[0].a.y')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(3)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(2)
})

test('assignment object attribute #3 spaces', () => {
  ludolfC.execute('a := [{a:{x:1, y:2}}]\n  a  [  0  ]  .  a  .  x  :=  3  \nb := a[0].a.x\nc := a[0].a.y')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(3)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(2)
})

test('assignment object attribute #3 no spaces', () => {
  ludolfC.execute('a := [{a:{x:1, y:2}}]\na[0].a.x:=3\nb := a[0].a.x\nc := a[0].a.y')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(3)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(2)
})

test('assignment object attribute #4 spaces and newlines', () => {
  ludolfC.execute('a := [{a:{x:1, y:2}}]\n  a  [  0  ]  .  a  .  x  :=  {  \n\n z : 3 \n\n  }  \nb := a[0].a.x.z\nc := a[0].a.y')
  expect(ludolfC.hasVariable('b')).toBe(true)
  expect(ludolfC.getVariable('b').type).toBe('NUMBER')
  expect(ludolfC.getVariable('b').value).toBe(3)
  expect(ludolfC.hasVariable('c')).toBe(true)
  expect(ludolfC.getVariable('c').type).toBe('NUMBER')
  expect(ludolfC.getVariable('c').value).toBe(2)
})

test('assignment error array size', () => {
  expect(() => ludolfC.execute('a := []\na.size := 1')).toThrow()
  expect(() => ludolfC.execute('a := []\na.Size := 1')).toThrow()
  expect(() => ludolfC.execute('a := []\na.velikost := 1')).toThrow()
  expect(() => ludolfC.execute('a := []\na.Velikost := 1')).toThrow()
  expect(() => ludolfC.execute('a := []\na.größe := 1')).toThrow()
  expect(() => ludolfC.execute('a := []\na.Größe := 1')).toThrow()
})