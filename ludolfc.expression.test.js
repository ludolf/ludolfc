const LudolfC = require('./ludolfc')
const interpret = new LudolfC();

test('expression number simplest', () => {
  expect(interpret.exec('1')).toBe(1)
})

test('expression number simplest #2', () => {
  expect(interpret.exec('123')).toBe(123)
})

test('expression number simplest #2', () => {
  expect(interpret.exec('123 + 1')).toBe(124)
})

test('expression number simplest #3', () => {
  expect(interpret.exec('123 + 123')).toBe(246)
})

test('expression number simplest #4', () => {
  expect(interpret.exec('(123 + (123))')).toBe(246)
})

test('expression number simplest #5', () => {
  expect(interpret.exec('(123 + (123)) * 2')).toBe(492)
})

test('expression boolean simplest', () => {
  expect(interpret.exec('true')).toBe(true)
})

test('expression boolean simplest #2', () => {
  expect(interpret.exec('false')).toBe(false)
})

test('expression boolean simplest #3', () => {
  expect(interpret.exec('false | true | false')).toBe(true)
})

test('expression boolean simplest #4', () => {
  expect(interpret.exec('false & true | false')).toBe(false)
})

test('expression boolean simplest #5', () => {
  expect(interpret.exec('!false & (true | false)')).toBe(true)
})

test('expression string simplest empty', () => {
  expect(interpret.exec('""')).toBe('')
})

test('expression string simplest space', () => {
  expect(interpret.exec('" "')).toBe(' ')
})

test('expression string simplest space #2', () => {
  expect(interpret.exec('"   "')).toBe('   ')
})

test('expression string simplest space #3', () => {
  expect(interpret.exec('"\n\n \n  \t\n"')).toBe('\n\n \n  \t\n')
})

test('expression string simplest space #4', () => {
  expect(interpret.exec('"\t\n\n \n  \t\n\t"')).toBe('\t\n\n \n  \t\n\t')
})

test('expression string simplest space #5', () => {
  expect(interpret.exec('"\t \n\n \n  \t\n \t"')).toBe('\t \n\n \n  \t\n \t')
})

test('expression string simplest space #5', () => {
  expect(interpret.exec('\n   \n\n \t\t "\t \n\n \n  \t\n \t"\t\n \n')).toBe('\t \n\n \n  \t\n \t')
})

test('expression string simplest', () => {
  expect(interpret.exec('"123"')).toBe('123')
})

test('expression string simplest #2', () => {
  expect(interpret.exec('"  a  b "')).toBe('  a  b ')
})

test('expression string simplest #3', () => {
  expect(interpret.exec('"\n\n\t\t  a  b \n\t\n"')).toBe('\n\n\t\t  a  b \n\t\n')
})

test('expression string simplest #4', () => {
  expect(interpret.exec('("")')).toBe('')
})

test('expression string simplest #5', () => {
  expect(interpret.exec('("(false & true | false)")')).toBe('(false & true | false)')
})

test('expression number var', () => {
  expect(interpret.exec('a := 1\n(123 + (a)) * 2')).toBe(248)
})

test('expression number var multiple lines', () => {
  expect(interpret.exec('\n\na := 1\n\n\n\n(123 + (a)) * 2\n\n')).toBe(248)
})

test('expression in the middle', () => {
  expect(interpret.exec('a := 1\n(123 + 1)\n\n5 + 2')).toBe(7)
})

test('expression var', () => {
  expect(interpret.exec('a := 123\na')).toBe(123)
})

test('expression var #2', () => {
  expect(interpret.exec('_1a := 123\n_1a')).toBe(123)
})

test('expression var spaces', () => {
  expect(interpret.exec('_1a := 123\n\n\n  _1a  \n\n')).toBe(123)
})

test('expression two vars', () => {
  expect(interpret.exec('a := 123\n\nb:=1\na')).toBe(123)
})

test('expression two vars #2', () => {
  expect(interpret.exec('a := 123\n\nb:=1\nb')).toBe(1)
})

test('expression two vars #3', () => {
  expect(interpret.exec('a := 123\n\nb:=1\n\nb+a')).toBe(124)
})