const LudolfC = require('./ludolfc')
const interpret = new LudolfC();

test('expression number simplest', () => {
  expect(interpret.exec('1').value).toBe(1)
})

test('expression number simplest #2', () => {
  expect(interpret.exec('123').value).toBe(123)
})

test('expression number simplest #2', () => {
  expect(interpret.exec('123 + 1').value).toBe(124)
})

test('expression number simplest #3', () => {
  expect(interpret.exec('123 + 123').value).toBe(246)
})

test('expression number simplest #4', () => {
  expect(interpret.exec('(123 + (123))').value).toBe(246)
})

test('expression number simplest #5', () => {
  expect(interpret.exec('(123 + (123)) * 2').value).toBe(492)
})

test('expression boolean simplest', () => {
  expect(interpret.exec('true').value).toBe(true)
})

test('expression boolean simplest #2', () => {
  expect(interpret.exec('false').value).toBe(false)
})

test('expression boolean simplest #3', () => {
  expect(interpret.exec('false | true | false').value).toBe(true)
})

test('expression boolean simplest #4', () => {
  expect(interpret.exec('false & true | false').value).toBe(false)
})

test('expression boolean simplest #5', () => {
  expect(interpret.exec('!false & (true | false)').value).toBe(true)
})

test('expression string simplest empty', () => {
  expect(interpret.exec('""').value).toBe('')
})

test('expression string simplest space', () => {
  expect(interpret.exec('" "').value).toBe(' ')
})

test('expression string simplest space #2', () => {
  expect(interpret.exec('"   "').value).toBe('   ')
})

test('expression string simplest space #3', () => {
  expect(interpret.exec('"\n\n \n  \t\n"').value).toBe('\n\n \n  \t\n')
})

test('expression string simplest space #4', () => {
  expect(interpret.exec('"\t\n\n \n  \t\n\t"').value).toBe('\t\n\n \n  \t\n\t')
})

test('expression string simplest space #5', () => {
  expect(interpret.exec('"\t \n\n \n  \t\n \t"').value).toBe('\t \n\n \n  \t\n \t')
})

test('expression string simplest space #5', () => {
  expect(interpret.exec('\n   \n\n \t\t "\t \n\n \n  \t\n \t"\t\n \n').value).toBe('\t \n\n \n  \t\n \t')
})

test('expression string simplest', () => {
  expect(interpret.exec('"123"').value).toBe('123')
})

test('expression string simplest #2', () => {
  expect(interpret.exec('"  a  b "').value).toBe('  a  b ')
})

test('expression string simplest #3', () => {
  expect(interpret.exec('"\n\n\t\t  a  b \n\t\n"').value).toBe('\n\n\t\t  a  b \n\t\n')
})

test('expression string simplest #4', () => {
  expect(interpret.exec('("")').value).toBe('')
})

test('expression string simplest #5', () => {
  expect(interpret.exec('("(false & true | false)")').value).toBe('(false & true | false)')
})

test('expression number var', () => {
  expect(interpret.exec('a := 1\n(123 + (a)) * 2').value).toBe(248)
})

test('expression number var multiple lines', () => {
  expect(interpret.exec('\n\na := 1\n\n\n\n(123 + (a)) * 2\n\n').value).toBe(248)
})

test('expression in the middle', () => {
  expect(interpret.exec('a := 1\n(123 + 1)\n\n5 + 2').value).toBe(7)
})

test('expression var', () => {
  expect(interpret.exec('a := 123\na').value).toBe(123)
})

test('expression var #2', () => {
  expect(interpret.exec('_1a := 123\n_1a').value).toBe(123)
})

test('expression var spaces', () => {
  expect(interpret.exec('_1a := 123\n\n\n  _1a  \n\n').value).toBe(123)
})

test('expression two vars', () => {
  expect(interpret.exec('a := 123\n\nb:=1\na').value).toBe(123)
})

test('expression two vars #2', () => {
  expect(interpret.exec('a := 123\n\nb:=1\nb').value).toBe(1)
})

test('expression two vars #3', () => {
  expect(interpret.exec('a := 123\n\nb:=1\n\nb+a').value).toBe(124)
})

test('expression func call', () => {
  expect(interpret.exec('1 - 123 . plus ( 10 ) . plus ( 55 ) . minus ( 88 ) . plus ( 123 . plus ( 10 ) . plus ( 55 ) . minus(88) ) + 2').value).toBe(-197)
})

test('expression func call vars', () => {
  expect(interpret.exec('_123:=123\n_10:=10\n1 - _123 . plus ( _10 ) . plus ( 55 ) . minus ( 88 ) . plus ( _123 . plus ( _10 ) . plus ( 55 ) . minus(88) ) + 2').value).toBe(-197)
})

test('expression func call grouping', () => {
  expect(interpret.exec('(123.plus((((10.plus(((50 + 5))))))) - 8) + 1 + 0.plus(1) + 1.plus(0)').value).toBe(183)
})

test('expression func call grouping spaces', () => {
  expect(interpret.exec('  ( 123 . plus ( ( ( ( 10 . plus ( (   (  50  +5 )  ) ) ) )   ) )  -   8   )+ 1+0. plus( 1  )+1   .plus (  0)').value).toBe(183)
})

test('expression func call double params', () => {
  expect(interpret.exec('1.sum(2.minus(1), (3.plus(1))).minus(1) + 1').value).toBe(6)
})

test('expression func call double params spaces', () => {
  expect(interpret.exec('    1 .   sum  (  2  .  minus ( 1   )   ,( 3  .plus ( 1)  )).minus   (1)+1').value).toBe(6)
})