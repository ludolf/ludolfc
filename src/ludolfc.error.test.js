const { LudolfC } = require('./ludolfc')
const ludolfC = new LudolfC()

test('error parse', () => {
  try {
    expect(() => ludolfC.execute('.')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(1)
  }
})

test('error parse #2', () => {
  try {
    expect(() => ludolfC.execute('2.')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(2)
  }
})

test('error parse #3', () => {
  try {
    expect(() => ludolfC.execute('1 +-')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(4)
  }
})

test('error parse #4', () => {
  try {
    expect(() => ludolfC.execute('1 2')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(2)
  }
})

test('error parse #5', () => {
  try {
    expect(() => ludolfC.execute('12\n1 2')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(2)
    expect(e.col).toBe(2)
  }
})

test('error parse #6', () => {
  try {
    expect(() => ludolfC.execute('1\n2\n#')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(3)
    expect(e.col).toBe(1)
  }
})

test('error parse #7', () => {
  try {
    expect(() => ludolfC.execute('1\n2\nwhile 1 {')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(3)
    expect(e.col).toBe(9)
  }
})

test('error parse #8', () => {
  try {
    expect(() => ludolfC.execute('1\n2\nwhile {}')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(3)
    expect(e.col).toBe(6)
  }
})

test('error parse #9', () => {
  try {
    expect(() => ludolfC.execute('while := 1')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(6)
  }
})

test('error parse #10', () => {
  try {
    expect(() => ludolfC.execute('if {}')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(3)
  }
})

test('error parse #11', () => {
  try {
    expect(() => ludolfC.execute('if else')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(3)
  }
})

test('error parse #12', () => {
  try {
    expect(() => ludolfC.execute('else {}')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(5)
  }
})

test('error interpret', () => {
  try {
    expect(() => ludolfC.execute('x')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(1)
  }
})

test('error interpret #2', () => {
  try {
    expect(() => ludolfC.execute('1 := 2')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(5)
  }
})

test('error interpret #3', () => {
  try {
    expect(() => ludolfC.execute('if true { x }')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(11)
  }
})

test('error interpret #4', () => {
  try {
    expect(() => ludolfC.execute('1 > true')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(8)
  }
})

test('error interpret #5', () => {
  try {
    expect(() => ludolfC.execute('f:=(){x}\nf()')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.line).toBe(1)
    expect(e.col).toBe(7)
  }
})

test('error interpret #6', () => {
  try {
    expect(() => ludolfC.execute('f:=(){\n1\nx}\nf()')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.line).toBe(3)
    expect(e.col).toBe(1)
  }
})

test('error interpret #7', () => {
  try {
    expect(() => ludolfC.execute('while true {\n1\nx}')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.line).toBe(3)
    expect(e.col).toBe(1)
  }
})

test('error interpret #8', () => {
  try {
    expect(() => ludolfC.execute('if false {\n1\nx\n}\nelse {\na := 123 + y\n}')).rejects.toThrow()
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.line).toBe(6)
    expect(e.col).toBe(12)
  }
})