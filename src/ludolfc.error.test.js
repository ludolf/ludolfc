const { LudolfC, lang } = require('./ludolfc')
const ludolfC = new LudolfC()

test('error parse', async () => {
  try {
    await ludolfC.execute('.')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(1)
    expect(e.col).toBe(1)
  }
})

test('error parse #1', async () => {
  try {
    await ludolfC.execute('+')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(1)
    expect(e.col).toBe(1)
  }
})

test('error parse #2', async () => {
  try {
    await ludolfC.execute('2.')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.EXPECTED_IDENTIFIER)
    expect(e.line).toBe(1)
    expect(e.col).toBe(2)
  }
})

test('error parse #3', async () => {
  try {
    await ludolfC.execute('1 +-')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEVEN_OPERATORS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(4)
  }
})

test('error parse #4', async () => {
  try {
    await ludolfC.execute('1 2')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(1)
    expect(e.col).toBe(2)
  }
})

test('error parse #5', async () => {
  try {
    await ludolfC.execute('12\n1 2')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(2)
    expect(e.col).toBe(2)
  }
})

test('error parse #6', async () => {
  try {
    await ludolfC.execute('1\n2\n#')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(3)
    expect(e.col).toBe(1)
  }
})

test('error parse #7', async () => {
  try {
    await ludolfC.execute('1\n2\nwhile 1 {')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.EXPECTED_SYMBOL)
    expect(e.line).toBe(4)  // expected body on the new line
    expect(e.col).toBe(1)
  }
})

test('error parse #8', async () => {
  try {
    await ludolfC.execute('1\n2\nwhile {}')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(3)
    expect(e.col).toBe(6)
  }
})

test('error parse #9', async () => {
  try {
    await ludolfC.execute('while := 1')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_KEYWORD)
    expect(e.line).toBe(1)
    expect(e.col).toBe(6)
  }
})

test('error parse #10', async () => {
  try {
    await ludolfC.execute('if {}')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(1)
    expect(e.col).toBe(3)
  }
})

test('error parse #11', async () => {
  try {
    await ludolfC.execute('if else')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(1)
    expect(e.col).toBe(3)
  }
})

test('error parse #12', async () => {
  try {
    await ludolfC.execute('else {}')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(1)
    expect(e.col).toBe(5)
  }
})

test('error parse #13', async () => {
  try {
    await ludolfC.execute('\n\n1 2')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(3)
    expect(e.col).toBe(2)
  }
})

test('error parse #14', async () => {
  try {
    await ludolfC.execute('//x\n//y\n1 2')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(3)
    expect(e.col).toBe(2)
  }
})

test('error interpret', async () => {
  try {
    await ludolfC.execute('x')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNREFERENCED_VARIABLE)
    expect(e.line).toBe(1)
    expect(e.col).toBe(1)
  }
})

test('error interpret #2', async () => {
  try {
    await ludolfC.execute('1 := 2')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.ACCESS_OPERATOR_EXPECTED)
    expect(e.line).toBe(1)
    expect(e.col).toBe(5)
  }
})

test('error interpret #3', async () => {
  try {
    await ludolfC.execute('if true { x }')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNREFERENCED_VARIABLE)
    expect(e.line).toBe(1)
    expect(e.col).toBe(10)
  }
})

test('error interpret #4', async () => {
  try {
    await ludolfC.execute('1 > true')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNMATCHING_BI_OPERATOR_SUBJECTS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(2)
  }
})

test('error interpret #5', async () => {
  try {
    await ludolfC.execute('f:=(){x}\nf()')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNREFERENCED_VARIABLE)
    expect(e.line).toBe(1)
    expect(e.col).toBe(6)
  }
})

test('error interpret #6', async () => {
  try {
    await ludolfC.execute('f:=(){\n1\nx}\nf()')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNREFERENCED_VARIABLE)
    expect(e.line).toBe(3)
    expect(e.col).toBe(1)
  }
})

test('error interpret #7', async () => {
  try {
    await ludolfC.execute('while true {\n1\nx}')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNREFERENCED_VARIABLE)
    expect(e.line).toBe(3)
    expect(e.col).toBe(1)
  }
})

test('error interpret #8', async () => {
  try {
    await ludolfC.execute('if false {\n1\nx\n}\nelse {\na := 123 + y\n}')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNREFERENCED_VARIABLE)
    expect(e.line).toBe(6)
    expect(e.col).toBe(11)
  }
})

test('error interpret wrong ops', async () => {
  try {
    await ludolfC.execute('1 + "a"')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNMATCHING_BI_OPERATOR_SUBJECTS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(2)
  }
})

test('error interpret wrong ops #2', async () => {
  try {
    await ludolfC.execute('"a" + []')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNMATCHING_BI_OPERATOR_SUBJECTS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(4)
  }
})

test('error interpret wrong ops #3', async () => {
  try {
    await ludolfC.execute('"a" + [1]')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNMATCHING_BI_OPERATOR_SUBJECTS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(4)
  }
})

test('error interpret wrong ops #4', async () => {
  try {
    await ludolfC.execute('"a" + {}')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNMATCHING_BI_OPERATOR_SUBJECTS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(4)
  }
})

test('error interpret wrong ops #5', async () => {
  try {
    await ludolfC.execute('"a" + {b:"b"}')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNMATCHING_BI_OPERATOR_SUBJECTS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(4)
  }
})

test('error interpret attribute not found', async () => {
  try {
    await ludolfC.execute('o:={}\no.a')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.ATTRIBUTE_NOT_FOUND)
    expect(e.line).toBe(2)
    expect(e.col).toBe(3)
  }
})

test('error interpret element not found', async () => {
  try {
    await ludolfC.execute('a:=[]\na[0]')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.ARRAY_INDEX_OUT_BOUNDS)
    expect(e.line).toBe(2)
    expect(e.col).toBe(4)
  }
})

test('error interpret long varname', async () => {
  try {
    await ludolfC.execute('abcdefghijklmn')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNREFERENCED_VARIABLE)
    expect(e.line).toBe(1)
    expect(e.col).toBe(1)
  }
})

test('error parse uneven ops', async () => {
  try {
    await ludolfC.execute('1+')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEVEN_OPERATORS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(2)
  }
})

test('error parse uneven ops #2', async () => {
  try {
    await ludolfC.execute('1-')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEVEN_OPERATORS)
    expect(e.line).toBe(1)
    expect(e.col).toBe(2)
  }
})

test('error parse uneven ops #3', async () => {
  try {
    await ludolfC.execute('1!')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isParseError).toBe(true)
    expect(e.id).toBe(lang.Errors.UNEXPECTED_SYMBOL)
    expect(e.line).toBe(1)
    expect(e.col).toBe(1)
  }
})

test('error interpret div by zero', async () => {
  try {
    await ludolfC.execute('1/0')
    expect(true).toBe(false)  // reaching this point is not expected
  } catch (e) {
    expect(e.isLangError).toBe(true)
    expect(e.isInterpretError).toBe(true)
    expect(e.id).toBe(lang.Errors.DIVISION_BY_ZERO)
    expect(e.line).toBe(1)
    expect(e.col).toBe(1)
  }
})