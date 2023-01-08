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

test('assignment simple bi expression minus', () => {
  interpret.exec('a := 1 - 2')
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

test('assignment simple bi expression division', () => {
  interpret.exec('a := 6 / 3')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('NUMBER')
  expect(interpret.variables.get('a').value).toBe(2)
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
  interpret.exec('_1 := 1\n_2 := 2\na := ((((((((_1 + 6)) % ((3) + _2)) > _1) != !((!false))) & (123 <= _1))) | ((12 + 10) * ---_1 = _1 * -22))')
  expect(interpret.variables.has('a')).toBe(true)
  expect(interpret.variables.get('a').type).toBe('BOOLEAN')
  expect(interpret.variables.get('a').value).toBe(true)
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
