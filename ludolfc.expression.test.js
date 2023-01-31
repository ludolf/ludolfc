const LudolfC = require('./ludolfc')
const ludolfC = new LudolfC();

test('expression number simplest', () => {
  expect(ludolfC.execute('1').value).toBe(1)
})

test('expression number simplest #2', () => {
  expect(ludolfC.execute('123').value).toBe(123)
})

test('expression number simplest #2', () => {
  expect(ludolfC.execute('123 + 1').value).toBe(124)
})

test('expression number simplest #3', () => {
  expect(ludolfC.execute('123 + 123').value).toBe(246)
})

test('expression number simplest #4', () => {
  expect(ludolfC.execute('(123 + (123))').value).toBe(246)
})

test('expression number simplest #5', () => {
  expect(ludolfC.execute('(123 + (123)) * 2').value).toBe(492)
})

test('expression boolean simplest', () => {
  expect(ludolfC.execute('true').value).toBe(true)
})

test('expression boolean simplest #2', () => {
  expect(ludolfC.execute('false').value).toBe(false)
})

test('expression boolean simplest #3', () => {
  expect(ludolfC.execute('false | true | false').value).toBe(true)
})

test('expression boolean simplest #4', () => {
  expect(ludolfC.execute('false & true | false').value).toBe(false)
})

test('expression boolean simplest #5', () => {
  expect(ludolfC.execute('!false & (true | false)').value).toBe(true)
})

test('expression string simplest empty', () => {
  expect(ludolfC.execute('""').value).toBe('')
})

test('expression string simplest space', () => {
  expect(ludolfC.execute('" "').value).toBe(' ')
})

test('expression string simplest space #2', () => {
  expect(ludolfC.execute('"   "').value).toBe('   ')
})

test('expression string simplest space #3', () => {
  expect(ludolfC.execute('"\n\n \n  \t\n"').value).toBe('\n\n \n  \t\n')
})

test('expression string simplest space #4', () => {
  expect(ludolfC.execute('"\t\n\n \n  \t\n\t"').value).toBe('\t\n\n \n  \t\n\t')
})

test('expression string simplest space #5', () => {
  expect(ludolfC.execute('"\t \n\n \n  \t\n \t"').value).toBe('\t \n\n \n  \t\n \t')
})

test('expression string simplest space #5', () => {
  expect(ludolfC.execute('\n   \n\n \t\t "\t \n\n \n  \t\n \t"\t\n \n').value).toBe('\t \n\n \n  \t\n \t')
})

test('expression string simplest', () => {
  expect(ludolfC.execute('"123"').value).toBe('123')
})

test('expression string simplest #2', () => {
  expect(ludolfC.execute('"  a  b "').value).toBe('  a  b ')
})

test('expression string simplest #3', () => {
  expect(ludolfC.execute('"\n\n\t\t  a  b \n\t\n"').value).toBe('\n\n\t\t  a  b \n\t\n')
})

test('expression string simplest #4', () => {
  expect(ludolfC.execute('("")').value).toBe('')
})

test('expression string simplest #5', () => {
  expect(ludolfC.execute('("(false & true | false)")').value).toBe('(false & true | false)')
})

test('expression number var', () => {
  expect(ludolfC.execute('a := 1\n(123 + (a)) * 2').value).toBe(248)
})

test('expression number var multiple lines', () => {
  expect(ludolfC.execute('\n\na := 1\n\n\n\n(123 + (a)) * 2\n\n').value).toBe(248)
})

test('expression error wrong var name', () => {
  expect(() => ludolfC.execute('1a')).toThrow()
})

test('expression error wrong value', () => {
  expect(() => ludolfC.execute('1 2')).toThrow()
})

test('expression error wrong operators', () => {
  expect(() => ludolfC.execute('1 + + 2')).toThrow()
})

test('expression error wrong uniop', () => {
  expect(() => ludolfC.execute('- 1')).toThrow()
})

test('expression error unknown biop', () => {
  expect(() => ludolfC.execute('1 @ 2')).toThrow()
})

test('expression error unknown uniop', () => {
  expect(() => ludolfC.execute('@1')).toThrow()
})

test('expression error wrong value #2', () => {
  expect(() => ludolfC.execute('a:=1\na 1')).toThrow()
})

test('expression error wrong value #3', () => {
  expect(() => ludolfC.execute('a:=1\n1 a')).toThrow()
})

test('expression error wrong value #4', () => {
  expect(() => ludolfC.execute('a:=1\b:=2\na b')).toThrow()
})

test('expression in the middle', () => {
  expect(ludolfC.execute('a := 1\n(123 + 1)\n\n5 + 2').value).toBe(7)
})

test('expression var', () => {
  expect(ludolfC.execute('a := 123\na').value).toBe(123)
})

test('expression var #2', () => {
  expect(ludolfC.execute('_1a := 123\n_1a').value).toBe(123)
})

test('expression var spaces', () => {
  expect(ludolfC.execute('_1a := 123\n\n\n  _1a  \n\n').value).toBe(123)
})

test('expression two vars', () => {
  expect(ludolfC.execute('a := 123\n\nb:=1\na').value).toBe(123)
})

test('expression two vars #2', () => {
  expect(ludolfC.execute('a := 123\n\nb:=1\nb').value).toBe(1)
})

test('expression two vars #3', () => {
  expect(ludolfC.execute('a := 123\n\nb:=1\n\nb+a').value).toBe(124)
})

test('expression func call', () => {
  expect(ludolfC.execute('1 - 123 . plus ( 10 ) . plus ( 55 ) . minus ( 88 ) . plus ( 123 . plus ( 10 ) . plus ( 55 ) . minus(88) ) + 2').value).toBe(-197)
})

test('expression func call vars', () => {
  expect(ludolfC.execute('_123:=123\n_10:=10\n1 - _123 . plus ( _10 ) . plus ( 55 ) . minus ( 88 ) . plus ( _123 . plus ( _10 ) . plus ( 55 ) . minus(88) ) + 2').value).toBe(-197)
})

test('expression func call grouping', () => {
  expect(ludolfC.execute('(123.plus((((10.plus(((50 + 5))))))) - 8) + 1 + 0.plus(1) + 1.plus(0)').value).toBe(183)
})

test('expression func call grouping spaces', () => {
  expect(ludolfC.execute('  ( 123 . plus ( ( ( ( 10 . plus ( (   (  50  +5 )  ) ) ) )   ) )  -   8   )+ 1+0. plus( 1  )+1   .plus (  0)').value).toBe(183)
})

test('expression func call double params', () => {
  expect(ludolfC.execute('1.sum(2.minus(1), (3.plus(1))).minus(1) + 1').value).toBe(6)
})

test('expression func call double params spaces', () => {
  expect(ludolfC.execute('    1 .   sum  (  2  .  minus ( 1   )   ,( 3  .plus ( 1)  )).minus   (1)+1').value).toBe(6)
})

test('expression func call quotings', () => {
  expect(ludolfC.execute('o:=8\n(o){(o)}((o))').value).toBe(8)
})

test('expression array definition one dimension', () => {
  expect(ludolfC.execute('[1,2]').value[1].value).toBe(2)
})

test('expression array definition one dimension #2', () => {
  expect(ludolfC.execute('[1, 2 \n ]').value[1].value).toBe(2)
})

test('expression array definition one dimension #3', () => {
  expect(ludolfC.execute(' x:=1  \n[  \n (0.plus(1) + 1), \n3.minus(x) +12,   10 + x.plus(x +x) \n ]   ').value[2].value).toBe(13)
})

test('expression array access two dimensions', () => {
  expect(ludolfC.execute('[[1,2],[3]][0,1]').value).toBe(2)
})

test('expression array access two dimensions #2', () => {
  expect(ludolfC.execute('[[1,2],[3]][0 + 0.plus(0), 123.minus(123) + 1]').value).toBe(2)
})

test('expression object access', () => {
  expect(ludolfC.execute('{a:1}.a').value).toBe(1)
  expect(ludolfC.execute('{o:{o:1},p:{o:2}}.o.o').value).toBe(1)
  expect(ludolfC.execute('{o:{o:1},p:{o:2}}.p.o').value).toBe(2)
  expect(ludolfC.execute('{o:{o:[1,2,[3,4]]}}.o.o[2,1].plus(100) * 2').value).toBe(208)
  expect(ludolfC.execute('{o:{o:[1,2,[3,4]]}}.o.o[2,1].plus(100) * 2 + {o:[1]}.o[0]').value).toBe(209)
  expect(ludolfC.execute('{o:{o:[1,2,[3,4]]}}.o.o[2,1].plus(100) * (1 + {o:[1]}.o[0])').value).toBe(208)
})

test('expression function call', () => {
  expect(ludolfC.execute('f := (){1}\nf()').value).toBe(1)
  expect(ludolfC.execute('f := (x){x}\nf(2)').value).toBe(2)
  expect(ludolfC.execute('f := (x){x+1}\nf(2)').value).toBe(3)
  expect(ludolfC.execute('f := (x,y){_ := y + 1\ny := x + 5\n_+y}\nf(1,2)').value).toBe(9)
  expect(ludolfC.execute('f := (x,y){o := { x:y, z:x }\ny := o.x + 5\no.z + y}\nf(1,2)').value).toBe(8)
})

test('expression object inner attributes', () => {
  expect(ludolfC.execute('o := { a:1, b:2, f:(){a+b} }\no.f()').value).toBe(3)
  expect(ludolfC.execute('o := { a:1, b:{ c:2, f:(){a+c} } }\no.b.f()').value).toBe(3)
})

test('expression array element assignment', () => {
  expect(ludolfC.execute('a := [1,2]\na[0] := 3\na[0]').value).toBe(3)
  expect(ludolfC.execute('o := {a:[[1],[2,3]]}\no.a[1][0] := 4\no.a[1][0]').value).toBe(4)
  expect(ludolfC.execute('a := [{a:[[1],[2,3]]}]\na[0].a[1][0] := 4\na[0].a[1][0]').value).toBe(4)
  expect(ludolfC.execute('a := [{a:[[1],[2,3]]}]\na[0].a[1][0] := 4 + a[0].a[1][0]\na[0].a[1][0]').value).toBe(6)
})

test('expression object attribute assignment', () => {
  expect(ludolfC.execute('a := {x:1, y:2}\na.x := 3\na.x').value).toBe(3)
  expect(ludolfC.execute('a := [{a:{x:1, y:2}}]\na[0].a.x := 3\na[0].a.x').value).toBe(3)
})

test('expression array size', () => {
  expect(ludolfC.execute('a := []\na.size').value).toBe(0)
  expect(ludolfC.execute('a := [1,2,3]\na.size').value).toBe(3)
  expect(ludolfC.execute('a := [[1,2],[3]]\na.size').value).toBe(2)
  expect(ludolfC.execute('a := [[1,2],[3]]\na[0].size').value).toBe(2)
  expect(ludolfC.execute('a := [[1,2],[3]]\na[1].size').value).toBe(1)
})

test('expression array size localized', () => {
  expect(ludolfC.execute('a := []\na.velikost').value).toBe(0)
  expect(ludolfC.execute('a := [1,2,3]\na.größe').value).toBe(3)
  expect(ludolfC.execute('a := [[1,2],[3]]\na.Größe').value).toBe(2)
})
