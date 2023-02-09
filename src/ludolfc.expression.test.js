const { LudolfC } = require('./ludolfc')
const ludolfC = new LudolfC()

test('expression number simplest', async () => {
  const result = await ludolfC.execute('1')
  expect(result.value).toBe(1)
})

test('expression number simplest #2', async () => {
  const result = await ludolfC.execute('123')
  expect(result.value).toBe(123)
})

test('expression number simplest #2', async () => {
  const result = await ludolfC.execute('123 + 1')
  expect(result.value).toBe(124)
})

test('expression number simplest #3', async () => {
  const result = await ludolfC.execute('123 + 123')
  expect(result.value).toBe(246)
})

test('expression number simplest #4', async () => {
  const result = await ludolfC.execute('(123 + (123))')
  expect(result.value).toBe(246)
})

test('expression number simplest #5', async () => {
  const result = await ludolfC.execute('(123 + (123)) * 2')
  expect(result.value).toBe(492)
})

test('expression boolean simplest', async () => {
  const result = await ludolfC.execute('true')
  expect(result.value).toBe(true)
})

test('expression boolean simplest #2', async () => {
  const result = await ludolfC.execute('false')
  expect(result.value).toBe(false)
})

test('expression boolean simplest #3', async () => {
  const result = await ludolfC.execute('false | true | false')
  expect(result.value).toBe(true)
})

test('expression boolean simplest #4', async () => {
  const result = await ludolfC.execute('false & true | false')
  expect(result.value).toBe(false)
})

test('expression boolean simplest #5', async () => {
  const result = await ludolfC.execute('!false & (true | false)')
  expect(result.value).toBe(true)
})

test('expression string simplest empty', async () => {
  const result = await ludolfC.execute('""')
  expect(result.value).toBe('')
})

test('expression string simplest space', async () => {
  const result = await ludolfC.execute('" "')
  expect(result.value).toBe(' ')
})

test('expression string simplest space #2', async () => {
  const result = await ludolfC.execute('"   "')
  expect(result.value).toBe('   ')
})

test('expression string simplest space #3', async () => {
  const result = await ludolfC.execute('"\n\n \n  \t\n"')
  expect(result.value).toBe('\n\n \n  \t\n')
})

test('expression string simplest space #4', async () => {
  const result = await ludolfC.execute('"\t\n\n \n  \t\n\t"')
  expect(result.value).toBe('\t\n\n \n  \t\n\t')
})

test('expression string simplest space #5', async () => {
  const result = await ludolfC.execute('"\t \n\n \n  \t\n \t"')
  expect(result.value).toBe('\t \n\n \n  \t\n \t')
})

test('expression string simplest space #5', async () => {
  const result = await ludolfC.execute('\n   \n\n \t\t "\t \n\n \n  \t\n \t"\t\n \n')
  expect(result.value).toBe('\t \n\n \n  \t\n \t')
})

test('expression string simplest', async () => {
  const result = await ludolfC.execute('"123"')
  expect(result.value).toBe('123')
})

test('expression string simplest #2', async () => {
  const result = await ludolfC.execute('"  a  b "')
  expect(result.value).toBe('  a  b ')
})

test('expression string simplest #3', async () => {
  const result = await ludolfC.execute('"\n\n\t\t  a  b \n\t\n"')
  expect(result.value).toBe('\n\n\t\t  a  b \n\t\n')
})

test('expression string simplest #4', async () => {
  const result = await ludolfC.execute('("")')
  expect(result.value).toBe('')
})

test('expression string simplest #5', async () => {
  const result = await ludolfC.execute('("(false & true | false)")')
  expect(result.value).toBe('(false & true | false)')
})

test('expression number var', async () => {
  const result = await ludolfC.execute('a := 1\n(123 + (a)) * 2')
  expect(result.value).toBe(248)
})

test('expression number var multiple lines', async () => {
  const result = await ludolfC.execute('\n\na := 1\n\n\n\n(123 + (a)) * 2\n\n')
  expect(result.value).toBe(248)
})

test('expression error wrong var name', () => {
  expect(() => ludolfC.execute('1a')).rejects.toThrow()
})

test('expression error wrong value', () => {
  expect(() => ludolfC.execute('1 2')).rejects.toThrow()
})

test('expression error wrong operators', () => {
  expect(() => ludolfC.execute('1 + + 2')).rejects.toThrow()
})

test('expression error wrong uniop', () => {
  expect(() => ludolfC.execute('- 1')).rejects.toThrow()
})

test('expression error unknown biop', () => {
  expect(() => ludolfC.execute('1 @ 2')).rejects.toThrow()
})

test('expression error unknown uniop', () => {
  expect(() => ludolfC.execute('@1')).rejects.toThrow()
})

test('expression error wrong value #2', () => {
  expect(() => ludolfC.execute('a:=1\na 1')).rejects.toThrow()
})

test('expression error wrong value #3', () => {
  expect(() => ludolfC.execute('a:=1\n1 a')).rejects.toThrow()
})

test('expression error wrong value #4', () => {
  expect(() => ludolfC.execute('a:=1\b:=2\na b')).rejects.toThrow()
})

test('expression in the middle', async () => {
  const result = await ludolfC.execute('a := 1\n(123 + 1)\n\n5 + 2')
  expect(result.value).toBe(7)
})

test('expression var', async () => {
  const result = await ludolfC.execute('a := 123\na')
  expect(result.value).toBe(123)
})

test('expression var #2', async () => {
  const result = await ludolfC.execute('_1a := 123\n_1a')
  expect(result.value).toBe(123)
})

test('expression var spaces', async () => {
  const result = await ludolfC.execute('_1a := 123\n\n\n  _1a  \n\n')
  expect(result.value).toBe(123)
})

test('expression two vars', async () => {
  const result = await ludolfC.execute('a := 123\n\nb:=1\na')
  expect(result.value).toBe(123)
})

test('expression two vars #2', async () => {
  const result = await ludolfC.execute('a := 123\n\nb:=1\nb')
  expect(result.value).toBe(1)
})

test('expression two vars #3', async () => {
  const result = await ludolfC.execute('a := 123\n\nb:=1\n\nb+a')
  expect(result.value).toBe(124)
})

test('expression func call', async () => {
  const result = await ludolfC.execute('1 - 123 . plus ( 10 ) . plus ( 55 ) . minus ( 88 ) . plus ( 123 . plus ( 10 ) . plus ( 55 ) . minus(88) ) + 2')
  expect(result.value).toBe(-197)
})

test('expression func call vars', async () => {
  const result = await ludolfC.execute('_123:=123\n_10:=10\n1 - _123 . plus ( _10 ) . plus ( 55 ) . minus ( 88 ) . plus ( _123 . plus ( _10 ) . plus ( 55 ) . minus(88) ) + 2')
  expect(result.value).toBe(-197)
})

test('expression func call grouping', async () => {
  const result = await ludolfC.execute('(123.plus((((10.plus(((50 + 5))))))) - 8) + 1 + 0.plus(1) + 1.plus(0)')
  expect(result.value).toBe(183)
})

test('expression func call grouping spaces', async () => {
  const result = await ludolfC.execute('  ( 123 . plus ( ( ( ( 10 . plus ( (   (  50  +5 )  ) ) ) )   ) )  -   8   )+ 1+0. plus( 1  )+1   .plus (  0)')
  expect(result.value).toBe(183)
})

test('expression func call double params', async () => {
  const result = await ludolfC.execute('1.sum(2.minus(1), (3.plus(1))).minus(1) + 1')
  expect(result.value).toBe(6)
})

test('expression func call double params spaces', async () => {
  const result = await ludolfC.execute('    1 .   sum  (  2  .  minus ( 1   )   ,( 3  .plus ( 1)  )).minus   (1)+1')
  expect(result.value).toBe(6)
})

test('expression func call quotings', async () => {
  const result = await ludolfC.execute('o:=8\n(o){(o)}((o))')
  expect(result.value).toBe(8)
})

test('expression array definition one dimension', async () => {
  const result = await ludolfC.execute('[1,2]')
  expect(result.value[1].value).toBe(2)
})

test('expression array definition one dimension #2', async () => {
  const result = await ludolfC.execute('[1, 2 \n ]')
  expect(result.value[1].value).toBe(2)
})

test('expression array definition one dimension #3', async () => {
  const result = await ludolfC.execute(' x:=1  \n[  \n (0.plus(1) + 1), \n3.minus(x) +12,   10 + x.plus(x +x) \n ]   ')
  expect(result.value[2].value).toBe(13)
})

test('expression array access two dimensions', async () => {
  const result = await ludolfC.execute('[[1,2],[3]][0,1]')
  expect(result.value).toBe(2)
})

test('expression array access two dimensions #2', async () => {
  const result = await ludolfC.execute('[[1,2],[3]][0 + 0.plus(0), 123.minus(123) + 1]')
  expect(result.value).toBe(2)
})

test('expression object access', async () => {
  const result1 = await ludolfC.execute('{a:1}.a')
  expect(result1.value).toBe(1)
  const result2 = await ludolfC.execute('{o:{o:1},p:{o:2}}.o.o')
  expect(result2.value).toBe(1)
  const result3 = await ludolfC.execute('{o:{o:1},p:{o:2}}.p.o')
  expect(result3.value).toBe(2)
  const result4 = await ludolfC.execute('{o:{o:[1,2,[3,4]]}}.o.o[2,1].plus(100) * 2')
  expect(result4.value).toBe(208)
  const result5 = await ludolfC.execute('{o:{o:[1,2,[3,4]]}}.o.o[2,1].plus(100) * 2 + {o:[1]}.o[0]')
  expect(result5.value).toBe(209)
  const result6 = await ludolfC.execute('{o:{o:[1,2,[3,4]]}}.o.o[2,1].plus(100) * (1 + {o:[1]}.o[0])')
  expect(result6.value).toBe(208)
})

test('expression function call', async () => {
  const result1 = await ludolfC.execute('f := (){1}\nf()')
  expect(result1.value).toBe(1)
  const result2 = await ludolfC.execute('f := (x){x}\nf(2)')
  expect(result2.value).toBe(2)
  const result3 = await ludolfC.execute('f := (x){x+1}\nf(2)')
  expect(result3.value).toBe(3)
  const result4 = await ludolfC.execute('f := (x,y){_ := y + 1\ny := x + 5\n_+y}\nf(1,2)')
  expect(result4.value).toBe(9)
  const result5 = await ludolfC.execute('f := (x,y){o := { x:y, z:x }\ny := o.x + 5\no.z + y}\nf(1,2)')
  expect(result5.value).toBe(8)
})

test('expression object inner attributes', async () => {
  const result1 = await ludolfC.execute('o := { a:1, b:2, f:(){a+b} }\no.f()')
  expect(result1.value).toBe(3)
  const result2 = await ludolfC.execute('o := { a:1, b:{ c:2, f:(){a+c} } }\no.b.f()')
  expect(result2.value).toBe(3)
})

test('expression array element assignment', async () => {
  const result1 = await ludolfC.execute('a := [1,2]\na[0] := 3\na[0]')
  expect(result1.value).toBe(3)
  const result2 = await ludolfC.execute('o := {a:[[1],[2,3]]}\no.a[1][0] := 4\no.a[1][0]')
  expect(result2.value).toBe(4)
  const result3 = await ludolfC.execute('a := [{a:[[1],[2,3]]}]\na[0].a[1][0] := 4\na[0].a[1][0]')
  expect(result3.value).toBe(4)
  const result4 = await ludolfC.execute('a := [{a:[[1],[2,3]]}]\na[0].a[1][0] := 4 + a[0].a[1][0]\na[0].a[1][0]')
  expect(result4.value).toBe(6)
})

test('expression object attribute assignment', async () => {
  const result1 = await ludolfC.execute('a := {x:1, y:2}\na.x := 3\na.x')
  expect(result1.value).toBe(3)
  const result2 = await ludolfC.execute('a := [{a:{x:1, y:2}}]\na[0].a.x := 3\na[0].a.x')
  expect(result2.value).toBe(3)
})

test('expression array size', async () => {
  const result1 = await ludolfC.execute('a := []\na.size')
  expect(result1.value).toBe(0)
  const result2 = await ludolfC.execute('a := [1,2,3]\na.size')
  expect(result2.value).toBe(3)
  const result3 = await ludolfC.execute('a := [[1,2],[3]]\na.size')
  expect(result3.value).toBe(2)
  const result4 = await ludolfC.execute('a := [[1,2],[3]]\na[0].size')
  expect(result4.value).toBe(2)
  const result5 = await ludolfC.execute('a := [[1,2],[3]]\na[1].size')
  expect(result5.value).toBe(1)
})

test('expression array size localized', async () => {
  const result1 = await ludolfC.execute('a := []\na.velikost')
  expect(result1.value).toBe(0)
  const result2 = await ludolfC.execute('a := [1,2,3]\na.größe')
  expect(result2.value).toBe(3)
  const result3 = await ludolfC.execute('a := [[1,2],[3]]\na.Größe')
  expect(result3.value).toBe(2)
})
