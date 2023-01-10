const LudolfC = require('./ludolfc')
const interpret = new LudolfC();

test('assignment number simplest', () => {
  interpret.exec('a := 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment number simplest #2', () => {
  interpret.exec('a := 256')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(256)
})

test('assignment number simplest #3', () => {
  interpret.exec('a := 1.2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1.2)
})

test('assignment number simplest #4', () => {
  interpret.exec('a := 256.12')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(256.12)
})

test('assignment number simplest underscore varname', () => {
  interpret.exec('_ := 256')
  expect(interpret.variables.has('_')).toBe(true)
  expect(interpret.variables.get('_').type).toBe('NUMBER')
  expect(interpret.variables.get('_').value).toBe(256)
})

test('assignment number simplest national chars', () => {
  interpret.exec('ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ := 256')
  expect(interpret.variables.has('ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ')).toBe(true)
  expect(interpret.variables.get('ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ').type).toBe('NUMBER')
  expect(interpret.variables.get('ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ').value).toBe(256)
})

test('assignment number simplest national chars #2', () => {
  interpret.exec('ěščřžýáíéúůüöäñ_1ĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ := 256')
  expect(interpret.variables.has('ěščřžýáíéúůüöäñ_1ĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ')).toBe(true)
  expect(interpret.variables.get('ěščřžýáíéúůüöäñ_1ĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ').type).toBe('NUMBER')
  expect(interpret.variables.get('ěščřžýáíéúůüöäñ_1ĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ').value).toBe(256)
})

test('assignment number simplest national chars #4', () => {
  interpret.exec('Ř := 256')
  expect(interpret.variables.has('Ř')).toBe(true)
  expect(interpret.variables.get('Ř').type).toBe('NUMBER')
  expect(interpret.variables.get('Ř').value).toBe(256)
})

test('assignment number simplest float', () => {
  interpret.exec('a := 25.16')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(25.16)
})

test('assignment number simplest no spaces', () => {
  interpret.exec('a:=1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment number simplest no spaces #2', () => {
  interpret.exec('a:=123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(123)
})

test('assignment number simplest space chars', () => {
  interpret.exec('\t\n\n \na\t \t := \t  1\t\t\n\n\n')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment number simplest loger varname', () => {
  interpret.exec('abc := 1')
  expect(interpret.variables.has('abc')).toBe(true)
  expect(interpret.variables.get('abc').type).toBe('NUMBER')
  expect(interpret.variables.get('abc').value).toBe(1)
})

test('assignment number simplest complex varname', () => {
  interpret.exec('__1abc_1__xy_QWERTY_ := 123')
  expect(interpret.variables.has('__1abc_1__xy_QWERTY_')).toBe(true)
  expect(interpret.variables.get('__1abc_1__xy_QWERTY_').type).toBe('NUMBER')
  expect(interpret.variables.get('__1abc_1__xy_QWERTY_').value).toBe(123)
})

test('assignment string simplest', () => {
  interpret.exec('a := "abc"')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('abc')
})

test('assignment string empty', () => {
  interpret.exec('a := ""')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('')
})

test('assignment string different quotation', () => {
  interpret.exec(`a := 'abc'`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('abc')
})

test('assignment string different quotation #2', () => {
  interpret.exec(`a := “abc”`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('abc')
})

test('assignment string different quotation #3', () => {
  interpret.exec(`a := ”abc”`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('abc')
})

test('assignment string different quotation #4', () => {
  interpret.exec(`a := “abc“`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('abc')
})

test('assignment string spaces', () => {
  interpret.exec(`a := "   a b  c    "`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('   a b  c    ')
})

test('assignment string spaces #2', () => {
  interpret.exec(`a := ("   a b  c    ")`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('   a b  c    ')
})

test('assignment string spaces #3', () => {
  interpret.exec(`a := ("   a b  c \t\n  \n  ")`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('   a b  c \t\n  \n  ')
})

test('assignment string tabs', () => {
  interpret.exec(`a := "\t   \t\t a b  c    \t"`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('\t   \t\t a b  c    \t')
})

test('assignment string newlines', () => {
  interpret.exec(`a := "\n\n\t   \t\t \n a b  c  \n\n  \t\n"`)
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('STRING')
  expect(interpret.variables.get('a').value).toBe('\n\n\t   \t\t \n a b  c  \n\n  \t\n')
})

test('assignment boolean true', () => {
  interpret.exec('b := true')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('BOOLEAN')
  expect(interpret.variables.get('b').value).toBe(true)
})

test('assignment boolean true upper', () => {
  interpret.exec('b := tRUe')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('BOOLEAN')
  expect(interpret.variables.get('b').value).toBe(true)
})

test('assignment boolean false', () => {
  interpret.exec('b := false')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('BOOLEAN')
  expect(interpret.variables.get('b').value).toBe(false)
})

test('assignment boolean false upper', () => {
  interpret.exec('b := FalsE')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('BOOLEAN')
  expect(interpret.variables.get('b').value).toBe(false)
})

test('assignment string true', () => {
  interpret.exec('b := "true"')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('STRING')
  expect(interpret.variables.get('b').value).toBe('true')
})

test('assignment string false', () => {
  interpret.exec('b := "false"')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('STRING')
  expect(interpret.variables.get('b').value).toBe('false')
})

test('assignment boolean true localized', () => {
  interpret.exec('b := pravda')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('BOOLEAN')
  expect(interpret.variables.get('b').value).toBe(true)
})

test('assignment boolean true upper localized', () => {
  interpret.exec('b := PravdA')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('BOOLEAN')
  expect(interpret.variables.get('b').value).toBe(true)
})

test('assignment boolean false localized', () => {
  interpret.exec('b := nepravda')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('BOOLEAN')
  expect(interpret.variables.get('b').value).toBe(false)
})

test('assignment boolean false upper localized', () => {
  interpret.exec('b := NEpravda')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('BOOLEAN')
  expect(interpret.variables.get('b').value).toBe(false)
})

test('assignment two statements', () => {
  interpret.exec('a := 123\nb := "false"')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(123)
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('STRING')
  expect(interpret.variables.get('b').value).toBe('false')
})

test('assignment three statements', () => {
  interpret.exec('a := 123\n\nb := "false"\n\nc := TRUE')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(123)
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('STRING')
  expect(interpret.variables.get('b').value).toBe('false')
  expect(interpret.variables.has('c')).toBe(true)
  expect(interpret.variables.get('c').type).toBe('BOOLEAN')
  expect(interpret.variables.get('c').value).toBe(true)
})

test('assignment simple var reference', () => {
  interpret.exec('a := 123\nb := a')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(123)
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('NUMBER')
  expect(interpret.variables.get('b').value).toBe(123)
})

test('assignment simple var self reference', () => {
  interpret.exec('a := 1\na := a')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})


test('assignment complex name var reference', () => {
  interpret.exec('a_1 := 123\na_2 := a_1')
  expect(interpret.variables.has('a_1')).toBe(true)
  expect(interpret.variables.get('a_1').type).toBe('NUMBER')
  expect(interpret.variables.get('a_1').value).toBe(123)
  expect(interpret.variables.has('a_2')).toBe(true)
  expect(interpret.variables.get('a_2').type).toBe('NUMBER')
  expect(interpret.variables.get('a_2').value).toBe(123)
})

test('assignment complex name var reference #2', () => {
  interpret.exec('_1 := 123\n_2 := _1')
  expect(interpret.variables.has('_1')).toBe(true)
  expect(interpret.variables.get('_1').type).toBe('NUMBER')
  expect(interpret.variables.get('_1').value).toBe(123)
  expect(interpret.variables.has('_2')).toBe(true)
  expect(interpret.variables.get('_2').type).toBe('NUMBER')
  expect(interpret.variables.get('_2').value).toBe(123)
})

test('assignment error wrong var name', () => {
  expect(() => interpret.exec('1 := 2')).toThrow()
})

test('assignment error wrong var name #2', () => {
  expect(() => interpret.exec('1a := 2')).toThrow()
})

test('assignment error wrong var name #3', () => {
  expect(() => interpret.exec('1_ := 2')).toThrow()
})

test('assignment error wrong var name #4', () => {
  expect(() => interpret.exec('1aa_bb := 2')).toThrow()
})

test('assignment error wrong var name #5', () => {
  expect(() => interpret.exec('💩 := 2')).toThrow()
})

test('assignment error wrong value', () => {
  expect(() => interpret.exec('a := 1b')).toThrow()
})

test('assignment error var wrong reference', () => {
  expect(() => interpret.exec('a := b')).toThrow()
})

test('assignment error var wrong reference two statements', () => {
  expect(() => interpret.exec('a := 1\nb := c')).toThrow()
})

test('assignment error var wrong reference three statements', () => {
  expect(() => interpret.exec('a := 1\nb := a\nb := c')).toThrow()
})

test('assignment error var wrong self reference', () => {
  expect(() => interpret.exec('a := a')).toThrow()
})

test('assignment error two vars wrong reference', () => {
  expect(() => interpret.exec('a := 123\nb := c')).toThrow()
})

test('assignment error unfinished', () => {
  expect(() => interpret.exec('a :')).toThrow()
})

test('assignment error space', () => {
  expect(() => interpret.exec('a : = 1')).toThrow()
})

test('assignment error space #2', () => {
  expect(() => interpret.exec('a :\n= 1')).toThrow()
})

test('assignment error ill', () => {
  expect(() => interpret.exec('a : 1')).toThrow()
})

test('assignment error ill #2', () => {
  expect(() => interpret.exec('a :: 1')).toThrow()
})

test('assignment error ill #3', () => {
  expect(() => interpret.exec('a :- 1')).toThrow()
})

test('assignment error incomplete', () => {
  expect(() => interpret.exec('a :=')).toThrow()
})

test('assignment error keywords', () => {
  expect(() => interpret.exec('true := 1')).toThrow()
  expect(() => interpret.exec('false := 1')).toThrow()
  expect(() => interpret.exec('if := 1')).toThrow()
  expect(() => interpret.exec('else := 1')).toThrow()
  expect(() => interpret.exec('while := 1')).toThrow()
})

test('assignment error keywords upper', () => {
  expect(() => interpret.exec('True := 1')).toThrow()
  expect(() => interpret.exec('False := 1')).toThrow()
  expect(() => interpret.exec('If := 1')).toThrow()
  expect(() => interpret.exec('Else := 1')).toThrow()
  expect(() => interpret.exec('While := 1')).toThrow()
})

test('assignment error keywords localized', () => {
  expect(() => interpret.exec('pravda := 1')).toThrow()
  expect(() => interpret.exec('nepravda := 1')).toThrow()
  expect(() => interpret.exec('pokud := 1')).toThrow()
  expect(() => interpret.exec('jinak := 1')).toThrow()
  expect(() => interpret.exec('dokud := 1')).toThrow()
})

test('assignment not keywords', () => {
  interpret.exec('_while := 123\nwhile_ := _while')
  expect(interpret.variables.has('_while')).toBe(true)
  expect(interpret.variables.get('_while').type).toBe('NUMBER')
  expect(interpret.variables.get('_while').value).toBe(123)
  expect(interpret.variables.has('while_')).toBe(true)
  expect(interpret.variables.get('while_').type).toBe('NUMBER')
  expect(interpret.variables.get('while_').value).toBe(123)
})

test('assignment simple bi expression plus', () => {
  interpret.exec('a := 1 + 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(3)
})

test('assignment simple bi expression plus #2', () => {
  interpret.exec('a := 1.5 + 2.5')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(4)
})

test('assignment simple bi expression minus', () => {
  interpret.exec('a := 1 - 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-1)
})

test('assignment simple bi expression minus #2', () => {
  interpret.exec('a := 1.5 - 2.5')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-1)
})

test('assignment simple bi expression multiplication', () => {
  interpret.exec('a := 2 * 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(6)
})

test('assignment simple bi expression multiplication #2', () => {
  interpret.exec('a := 2.5 * 3.5')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(8.75)
})

test('assignment simple bi expression division', () => {
  interpret.exec('a := 6 / 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(2)
})

test('assignment simple bi expression division #2', () => {
  interpret.exec('a := 6.8 / 3.2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(2.125)
})

test('assignment simple bi expression gt', () => {
  interpret.exec('a := 4 > 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression ge', () => {
  interpret.exec('a := 3 >= 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression lt', () => {
  interpret.exec('a := 2 < 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression le', () => {
  interpret.exec('a := 2 <= 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression ne', () => {
  interpret.exec('a := 2 != 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression ne false', () => {
  interpret.exec('a := 2 != 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple bi expression eq', () => {
  interpret.exec('a := 2 = 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression eq false', () => {
  interpret.exec('a := 2 = 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple bi expression and', () => {
  interpret.exec('a := true & true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression and false', () => {
  interpret.exec('a := true & false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple bi expression or', () => {
  interpret.exec('a := true | false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression or false', () => {
  interpret.exec('a := false | false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple bi expression no spaces', () => {
  interpret.exec('a:=2+3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(5)
})

test('assignment simple bi expression no spaces three members', () => {
  interpret.exec('a:=2+3+1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(6)
})

test('assignment simple bi expression no spaces two-chars op three members', () => {
  interpret.exec('a:=2!=3!=1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple bi expression three members', () => {
  interpret.exec('a := 2 + 3 - 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(4)
})

test('assignment simple bi expression three members ref', () => {
  interpret.exec('a := 1\nb := 2 + 3 - a')
  expect(interpret.variables.has('b')).toBe(true)
  expect(interpret.variables.get('b').type).toBe('NUMBER')
  expect(interpret.variables.get('b').value).toBe(4)
})

test('assignment simple bi expression three members ref #2', () => {
  interpret.exec('a := 1\nb := a + 1\nc := a + b + 3')
  expect(interpret.variables.has('c')).toBe(true)
  expect(interpret.variables.get('c').type).toBe('NUMBER')
  expect(interpret.variables.get('c').value).toBe(6)
})

test('assignment simple uni expression', () => {
  interpret.exec('a := !false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression', () => {
  interpret.exec('a := !false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression no spaces', () => {
  interpret.exec('a:=!false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression two members', () => {
  interpret.exec('a := !false | !true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression two members #2', () => {
  interpret.exec('a := !false & !true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple uni expression three members', () => {
  interpret.exec('a := !false & !true | !false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression three members #2', () => {
  interpret.exec('a := 2 > 3 | !false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression three members #3', () => {
  interpret.exec('a := 2 < 3 | !false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression three members #4', () => {
  interpret.exec('a := 2 > 3 | !true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple uni expression three members #5', () => {
  interpret.exec('a := 2 >= 3 | !true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple uni expression three members #6', () => {
  interpret.exec('a := 2 <= 3 | !true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression var three members', () => {
  interpret.exec('tr := true\nf := false\na := !f & !tr | !f')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple double uni expression', () => {
  interpret.exec('a := !!true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple double uni expression var', () => {
  interpret.exec('t := true\na := !!t')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple double uni expression var three members', () => {
  interpret.exec('tr := true\nf := false\na := !!tr & !!f | !!tr')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple triple uni expression', () => {
  interpret.exec('a := !!!true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple triple uni expression', () => {
  interpret.exec('a := !!!false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression minus', () => {
  interpret.exec('a := -1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-1)
})

test('assignment simple uni expression minus #2', () => {
  interpret.exec('a := -123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-123)
})

test('assignment simple uni expression minus #3', () => {
  interpret.exec('a := 123 > -123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression minus #4', () => {
  interpret.exec('a := -123 > -123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple uni expression minus #4', () => {
  interpret.exec('a := -123 >= -123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression minus #5', () => {
  interpret.exec('a:=-123>=-123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression minus #6', () => {
  interpret.exec('a := --123 >= --123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression minus #7', () => {
  interpret.exec('a := --123 >= ---123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment simple uni expression minus #8', () => {
  interpret.exec('a := ---123 >= --123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment simple uni expression minus #9', () => {
  interpret.exec('a :=---123')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-123)
})

test('assignment op precedence', () => {
  interpret.exec('a := 3 / 9 * 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment op precedence #2', () => {
  interpret.exec('a := 1 + 2 * 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(7)
})

test('assignment op precedence #3', () => {
  interpret.exec('a := 1 * 2 + 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(5)
})

test('assignment op precedence #4', () => {
  interpret.exec('a := 1 + 6 / 3 + 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(5)
})

test('assignment op precedence #5', () => {
  interpret.exec('a := 1 + 6 % 3 + 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(3)
})

test('assignment op precedence #6', () => {
  interpret.exec('a := 1 + 6 % 3 + 2 > 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment op precedence #7', () => {
  interpret.exec('a := 1 + 6 % 3 + 2 > 1 != false')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment op precedence #8', () => {
  interpret.exec('a := 1 + 6 % 3 + 2 > 1 != false & 2 <= 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment op precedence #9', () => {
  interpret.exec('a := 1 + 6 % 3 + 2 > 1 != false & 1 <= 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment op precedence #10', () => {
  interpret.exec('a := 1 + 6 % 3 + 2 > 1 != false & 123 <= 1 | 12 + 11 * -1 = 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment op precedence #11', () => {
  interpret.exec('a := 1 + 6 % 3 + 2 > 1 != !false & 123 <= 1 | 12 + 11 * -1 >= 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment op precedence #12', () => {
  interpret.exec('x := 2\na := 1 + 6 % 3 + x > 1 != !false & 123 <= 1 | 12 + 11 * -1 >= x')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment op precedence #13', () => {
  interpret.exec('_1 := 1\n_2 := 2\na := _1 + 6 % 3 + _2 > _1 != false & 123 <= _1 | 12 + 11 * -_1 = _1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment op precedence #14', () => {
  interpret.exec('_1 := 1\n_2 := 2\na := _1 + 6 % 3 + _2 > _1 != !!false & 123 <= _1 | 12 + 11 * ---_1 = _1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment op precedence #15', () => {
  interpret.exec('a := 1 + 2 + 3 * 3 - 2 - 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(9)
})

test('assignment op precedence #16', () => {
  interpret.exec('a := 1 + 2 + 3 * 3 / 3 * 3 - 2 - 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(9)
})

test('assignment op precedence #17', () => {
  interpret.exec('a := --1 + 1 - -1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(3)
})

test('assignment op precedence #18', () => {
  interpret.exec('a := -1 - --1 * -1 * -1 - -1 - --1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-2)
})

test('assignment grouping', () => {
  interpret.exec('a := (1)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment grouping #2', () => {
  interpret.exec('a := ((1))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment grouping #3', () => {
  interpret.exec('a := (((123)))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(123)
})

test('assignment grouping precedence', () => {
  interpret.exec('a := (1 + 6) / (3 + 2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1.4)
})

test('assignment grouping precedence #2', () => {
  interpret.exec('a := ((1 + 6)) / (3 + 2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1.4)
})

test('assignment grouping precedence #3', () => {
  interpret.exec('a := (((1 + 6)) / (3 + 2))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1.4)
})

test('assignment grouping precedence #4', () => {
  interpret.exec('a := (1 + (2) + 3)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(6)
})

test('assignment grouping precedence #5', () => {
  interpret.exec('_1 := 1\na := (((_1) + 3) / ((3) + _1))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment grouping precedence #6', () => {
  interpret.exec('_1 := (1) + 0\na := (((_1) + 3) / ((3) + _1))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment grouping precedence #7', () => {
  interpret.exec('_1 := 1\n_2 := 2\na := ((((((((_1 + 6)) % ((3) + _2)) > _1) != !((!false))) & (123.23 <= _1))) | ((12 + 10) * ---_1 = _1 * -22))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment grouping precedence #8', () => {
  interpret.exec('a := (1.5 + (2.6) + 3.7)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(7.8)
})

test('assignment error grouping', () => {
  expect(() => interpret.exec('a := ((1)')).toThrow()
})

test('assignment error grouping #2', () => {
  expect(() => interpret.exec('a := (1))')).toThrow()
})

test('assignment error grouping #3', () => {
  expect(() => interpret.exec('a := (1')).toThrow()
})

test('assignment error grouping #4', () => {
  expect(() => interpret.exec('a := 1)')).toThrow()
})

test('assignment error grouping #5', () => {
  expect(() => interpret.exec('a := (1+)(2)')).toThrow()
})

test('assignment error grouping #6', () => {
  expect(() => interpret.exec('a := (1 + (2 + 3)')).toThrow()
})

test('assignment error grouping #7', () => {
  expect(() => interpret.exec('a := ()')).toThrow()
})

test('assignment error grouping #8', () => {
  expect(() => interpret.exec('a := (())')).toThrow()
})

test('assignment func simple', () => {
  interpret.exec('a := 1.plus(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(3)
})

test('assignment func simple #2', () => {
  interpret.exec('a := 1.plus(2) + 4')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(7)
})

test('assignment func simple #3', () => {
  interpret.exec('a := 5 + 1.plus(2) + 4')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(12)
})

test('assignment func simple #4', () => {
  interpret.exec('a := !true.neg()')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #5', () => {
  interpret.exec('a := !!true.neg()')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #6', () => {
  interpret.exec('a := !1.gt(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #7', () => {
  interpret.exec('a := !!1.gt(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #8', () => {
  interpret.exec('a := !!1.gt(2) | true')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #9', () => {
  interpret.exec('a := false & !1.gt(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #10', () => {
  interpret.exec('a := 123.plus(23) + 12.minus(11) * 12.minus(10)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(148)
})

test('assignment func simple #11', () => {
  interpret.exec('a := 123.plus(12.minus(10) * 12.minus(9))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(129)
})

test('assignment func simple #12', () => {
  interpret.exec('a := 123.plus(10).plus(55)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(188)
})

test('assignment func simple #13', () => {
  interpret.exec('a := 123.plus(10).plus(55).minus(88) + 123.plus(10).plus(55).minus(88)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(200)
})

test('assignment func simple #14', () => {
  interpret.exec('a := 123.plus(10).plus(55).minus(88).plus(123.plus(10).plus(55).minus(88))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(200)
})

test('assignment func simple #15', () => {
  interpret.exec('a := 1 - 123.plus(10).plus(55).minus(88).plus(123.plus(10).plus(55).minus(88)) + 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-197)
})

test('assignment func simple #15 spaces', () => {
  interpret.exec('a := 1 - 123 . plus ( 10 ) . plus ( 55 ) . minus ( 88 ) . plus ( 123 . plus ( 10 ) . plus ( 55 ) . minus(88) ) + 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-197)
})

test('assignment func simple #15 vars', () => {
  interpret.exec('_123:=123\n_10:=10\na := 1 - _123 . plus ( _10 ) . plus ( 55 ) . minus ( 88 ) . plus ( _123 . plus ( _10 ) . plus ( 55 ) . minus(88) ) + 2')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(-197)
})

test('assignment func simple #16', () => {
  interpret.exec('a := 123.plus(10.plus(55))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(188)
})

test('assignment func simple #16 grouping', () => {
  interpret.exec('a := 123.plus((((10.plus(55)))))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(188)
})

test('assignment func simple #16 grouping #2', () => {
  interpret.exec('a := 123.plus((((10.plus(((50 + 5)))))))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(188)
})

test('assignment func simple #16 grouping #3', () => {
  interpret.exec('a := (123.plus((((10.plus(((50 + 5))))))) - 8) + 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(181)
})

test('assignment func simple #17', () => {
  interpret.exec('a := 5.mod(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(1)
})

test('assignment func simple #18', () => {
  interpret.exec('a := 5.mult(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(10)
})

test('assignment func simple #19', () => {
  interpret.exec('a := 5.div(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(2.5)
})

test('assignment func simple #20', () => {
  interpret.exec('a := 2.lt(5)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #21', () => {
  interpret.exec('a := 5.lt(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #22', () => {
  interpret.exec('a := 2.gt(5)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #23', () => {
  interpret.exec('a := 5.gt(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #24', () => {
  interpret.exec('a := 5.ge(2)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #25', () => {
  interpret.exec('a := 5.ge(5)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #26', () => {
  interpret.exec('a := 5.ge(4)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #27', () => {
  interpret.exec('a := 4.le(4)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #28', () => {
  interpret.exec('a := 4.le(3)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #29', () => {
  interpret.exec('a := 4.le(5)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #30', () => {
  interpret.exec('a := 4.eq(5)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #31', () => {
  interpret.exec('a := 5.eq(5)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #32', () => {
  interpret.exec('a := 5.eq(4)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #33', () => {
  interpret.exec('a := 5.ne(4)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #34', () => {
  interpret.exec('a := 4.ne(5)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #35', () => {
  interpret.exec('a := 5.ne(5)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #36', () => {
  interpret.exec('a := true.and(true)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #37', () => {
  interpret.exec('a := true.and(false)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #38', () => {
  interpret.exec('a := false.and(false)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #39', () => {
  interpret.exec('a := false.or(false)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #40', () => {
  interpret.exec('a := false.or(true)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #41', () => {
  interpret.exec('a := false.xor(true)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #42', () => {
  interpret.exec('a := false.xor(false)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #43', () => {
  interpret.exec('a := false.nand(false)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #44', () => {
  interpret.exec('a := false.nand(true)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #45', () => {
  interpret.exec('a := true.nand(true)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #46', () => {
  interpret.exec('a := true.neg()')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(false)
})

test('assignment func simple #47', () => {
  interpret.exec('a := false.neg()')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
})

test('assignment func simple #48', () => {
  interpret.exec('a := 123.plus(((12)))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(135)
})

test('assignment func multiple params', () => {
  interpret.exec('a := (123.plus((((10.plus(((50 + 5))))))) - 8) + 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(181)
})

test('assignment func double params', () => {
  interpret.exec('a := 1.sum(2,3)')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(6)
})

test('assignment func double params spaces', () => {
  interpret.exec('a := 1.sum(  2   ,    3    )')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(6)
})

test('assignment func double params #2', () => {
  interpret.exec('a := 1.sum((2), (3))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(6)
})

test('assignment func double params #3', () => {
  interpret.exec('a := 1.sum((2 - 1), (3 + 1))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(6)
})

test('assignment func double params #4', () => {
  interpret.exec('a := 1.sum(2.minus(1), (3.plus(1))).minus(1) + 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(6)
})

test('assignment func triple params', () => {
  interpret.exec('a := 123.sum(12,2,55) + 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(193)
})

test('assignment func triple params #2', () => {
  interpret.exec('a := 1.sum(2.minus(1), (3.plus(1)), 123).minus(1) + 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(129)
})

test('assignment func triple params var', () => {
  interpret.exec('x := 123\na := x.sum(2.minus(1), (3.plus(1)), x).minus(1) + 1')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(251)
})

test('assignment error func multi params', () => {
  expect(() => interpret.exec('a := 1.sum((2,3))')).toThrow()
})

test('assignment error func multi params #2', () => {
  expect(() => interpret.exec('a := 1.sum((2,3)')).toThrow()
})

test('assignment error func multi params #3', () => {
  expect(() => interpret.exec('a := 1.sum((2, 3)')).toThrow()
})

test('assignment error func multi params #4', () => {
  expect(() => interpret.exec('a := 1.sum((123, (2), 3))')).toThrow()
})

test('assignment error func multi params #5', () => {
  expect(() => interpret.exec('a := 1.sum(123, (2, 3))')).toThrow()
})

test('assignment error func multi params #6', () => {
  expect(() => interpret.exec('a := 1.sum((123, 2), 3)')).toThrow()
})

test('assignment error func not exists', () => {
  expect(() => interpret.exec('a := 1.xxx()')).toThrow()
})

test('assignment error func not exists #2', () => {
  expect(() => interpret.exec('a := 1.xxx(123)')).toThrow()
})

test('assignment error func not exists #3', () => {
  expect(() => interpret.exec('a := 1.xxx(12,3)')).toThrow()
})

test('assignment error attribute not exists', () => {
  expect(() => interpret.exec('a := 1.xxx')).toThrow()
})

test('assignment error attribute not exists #2', () => {
  expect(() => interpret.exec('a := 1\nb := a.xxx')).toThrow()
})

test('assignment array empty', () => {
  interpret.exec('a := []')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toEqual(expect.arrayContaining([]))
})

test('assignment array one dimension', () => {
  interpret.exec('a := [1]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(1)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(1)
})

test('assignment array one dimension #2', () => {
  interpret.exec('a := [1,2,3]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(3)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(2)
  expect(interpret.variables.get('a').value[2].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[2].value).toBe(3)
})

test('assignment array one dimension #2 spaces', () => {
  interpret.exec('a := [  1 ,2  ,   3   ]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(3)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(2)
  expect(interpret.variables.get('a').value[2].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[2].value).toBe(3)
})

test('assignment array one dimension #2 newlines', () => {
  interpret.exec('a := [\n1\n,\n2]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(2)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(2)
})

test('assignment array one dimension #2 newlines #2', () => {
  interpret.exec('a := [\n1\n,\n2\n,\n3\n]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(3)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(2)
  expect(interpret.variables.get('a').value[2].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[2].value).toBe(3)
})

test('assignment array one dimension #2 newlines and space', () => {
  interpret.exec('a := [ \n  1\n ,\n2  \n,\n   3   \n ]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(3)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(2)
  expect(interpret.variables.get('a').value[2].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[2].value).toBe(3)
})

test('assignment array one dimension #3', () => {
  interpret.exec('a := [0.plus(1) + 1, 3.minus(1) +12, 10 + 2.plus(1)]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(3)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(2)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(14)
  expect(interpret.variables.get('a').value[2].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[2].value).toBe(13)
})

test('assignment array one dimension #4', () => {
  interpret.exec('x := 1\na := [(0.plus(1) + 1), 3.minus(x) +12, 10 + x.plus(x +x)]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(3)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(2)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(14)
  expect(interpret.variables.get('a').value[2].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[2].value).toBe(13)
})

test('assignment array one dimension #5', () => {
  interpret.exec('x := 1\na := ([(0.plus(1) + 1), 3.minus(x) +12, 10 + x.plus(x +x)])')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(3)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(2)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(14)
  expect(interpret.variables.get('a').value[2].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[2].value).toBe(13)
})

test('assignment array one dimension heterogen', () => {
  interpret.exec('a := [123, "123", 123 > 100]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(3)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(123)
  expect(interpret.variables.get('a').value[1].type).toBe('STRING')
  expect(interpret.variables.get('a').value[1].value).toBe('123')
  expect(interpret.variables.get('a').value[2].type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value[2].value).toBe(true)
})

test('assignment array one dimension concat', () => {
  interpret.exec('a := [1,2].concat([3,4])')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(4)
  expect(interpret.variables.get('a').value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value).toBe(2)
  expect(interpret.variables.get('a').value[2].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[2].value).toBe(3)
  expect(interpret.variables.get('a').value[3].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[3].value).toBe(4)
})

test('assignment array two dimensions', () => {
  interpret.exec('a := [[1,2],[3]]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(2)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(2)
  expect(interpret.variables.get('a').value[0].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[0].value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[1].value).toBe(2)
  
  expect(interpret.variables.get('a').value[1].value).toHaveLength(1)
  expect(interpret.variables.get('a').value[1].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value[0].value).toBe(3)
})

test('assignment array two dimensions spaces', () => {
  interpret.exec('  a := [  [  1  ,  2    ]   ,  [    3   ]   ]   ')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(2)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(2)
  expect(interpret.variables.get('a').value[0].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[0].value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[1].value).toBe(2)
  
  expect(interpret.variables.get('a').value[1].value).toHaveLength(1)
  expect(interpret.variables.get('a').value[1].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value[0].value).toBe(3)
})

test('assignment array two dimensions newlines', () => {
  interpret.exec('a := [\n[\n1\n]\n]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(1)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(1)
  expect(interpret.variables.get('a').value[0].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[0].value).toBe(1)
})

test('assignment array two dimensions spaces and newlines', () => {
  interpret.exec('a := [ \n [ \n 1 \n ] \n ] ')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(1)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(1)
  expect(interpret.variables.get('a').value[0].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[0].value).toBe(1)
})

test('assignment array two dimensions spaces and newlines #2', () => {
  interpret.exec('a := [\n[\n1\n,\n2\n\n]\n]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(1)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(2)
  expect(interpret.variables.get('a').value[0].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[0].value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[1].value).toBe(2)
})

test('assignment array two dimensions spaces and newlines #3', () => {
  interpret.exec('  a := [\n  [\n  1  \n, \n 2 \n  \n ] \n  ,\n\n  [\n\n    3\n  \n ] \n\n \n ] \n  ')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(2)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(2)
  expect(interpret.variables.get('a').value[0].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[0].value).toBe(1)
  expect(interpret.variables.get('a').value[0].value[1].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[0].value[1].value).toBe(2)
  
  expect(interpret.variables.get('a').value[1].value).toHaveLength(1)
  expect(interpret.variables.get('a').value[1].value[0].type).toBe('NUMBER')
  expect(interpret.variables.get('a').value[1].value[0].value).toBe(3)
})

test('assignment array two empty dimensions', () => {
  interpret.exec('a := [[]]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(1)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(0)
})

test('assignment array two empty dimensions #2', () => {
  interpret.exec('a := [[],[]]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(2)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(0)

  expect(interpret.variables.get('a').value[1].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[1].value).toHaveLength(0)
})

test('assignment array three empty dimensions', () => {
  interpret.exec('a := [[[]],[]]')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(2)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(1)

  expect(interpret.variables.get('a').value[0].value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value[0].value).toHaveLength(0)

  expect(interpret.variables.get('a').value[1].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[1].value).toHaveLength(0)
})

test('assignment array three empty dimensions spaces and newlines', () => {
  interpret.exec('a := [ \n \n [  \n\n [ \n \n ] \n \n ] \n \n , \n \n [ \n \n ] \n \n ] \n \n ')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('ARRAY')
  expect(interpret.variables.get('a').value).toHaveLength(2)

  expect(interpret.variables.get('a').value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value).toHaveLength(1)

  expect(interpret.variables.get('a').value[0].value[0].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[0].value[0].value).toHaveLength(0)

  expect(interpret.variables.get('a').value[1].type).toBe('ARRAY')
  expect(interpret.variables.get('a').value[1].value).toHaveLength(0)
})

test('assignment error array', () => {
  expect(() => interpret.exec('a := [')).toThrow()
  expect(() => interpret.exec('a := ]')).toThrow()
  expect(() => interpret.exec('a := []]')).toThrow()
  expect(() => interpret.exec('a := [[]')).toThrow()
  expect(() => interpret.exec('a := [1 2]')).toThrow()
  expect(() => interpret.exec('a := [[][]]')).toThrow()
  expect(() => interpret.exec('a := [,]')).toThrow()
  expect(() => interpret.exec('a := [,]')).toThrow()
  expect(() => interpret.exec('a := [,]')).toThrow()
  expect(() => interpret.exec('a := [[],]')).toThrow()
  expect(() => interpret.exec('a := [[,]]')).toThrow()
  expect(() => interpret.exec('a := [[,]')).toThrow()
  expect(() => interpret.exec('a := [1,]')).toThrow()
  expect(() => interpret.exec('a := [1,2,]')).toThrow()  
})