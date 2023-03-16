const fs = require('fs')
const { LudolfC, lang } = require('./ludolfc')
const ludolfC = new LudolfC({
  Math: new lang.Object({floor: new lang.NativeFunction(x => new lang.Number(Math.floor(x.value)))})
})

test('eight times zero', async () => {
  await ludolfC.execute(fs.readFileSync('./test/eight.lc'))
  expect(ludolfC.hasVariable('o')).toBe(true)
  expect(ludolfC.getVariable('o').type).toBe('NUMBER')
  expect(ludolfC.getVariable('o').value).toBe(0)

  expect(ludolfC.hasVariable('_1')).toBe(true)
  expect(ludolfC.getVariable('_1').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_1').value).toBe(0)
  expect(ludolfC.hasVariable('_2')).toBe(true)
  expect(ludolfC.getVariable('_2').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_2').value).toBe(0)
  expect(ludolfC.hasVariable('_3')).toBe(true)
  expect(ludolfC.getVariable('_3').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_3').value).toBe(0)
  expect(ludolfC.hasVariable('_4')).toBe(true)
  expect(ludolfC.getVariable('_4').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_4').value).toBe(0)
  expect(ludolfC.hasVariable('_5')).toBe(true)
  expect(ludolfC.getVariable('_5').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_5').value).toBe(0)
  expect(ludolfC.hasVariable('_6')).toBe(true)
  expect(ludolfC.getVariable('_6').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_6').value).toBe(0)
  expect(ludolfC.hasVariable('_7')).toBe(true)
  expect(ludolfC.getVariable('_7').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_7').value).toBe(0)
  expect(ludolfC.hasVariable('_8')).toBe(true)
  expect(ludolfC.getVariable('_8').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_8').value).toBe(0)
  expect(ludolfC.hasVariable('_9')).toBe(true)
  expect(ludolfC.getVariable('_9').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_9').value).toBe(-1)
})

test('bubblesort', async () => {
  await ludolfC.execute(fs.readFileSync('./test/bubblesort.lc'))
  expect(ludolfC.hasVariable('inputArr')).toBe(true)
  expect(ludolfC.hasVariable('_0')).toBe(true)
  expect(ludolfC.getVariable('_0').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_0').value).toBe(1)
  expect(ludolfC.hasVariable('_1')).toBe(true)
  expect(ludolfC.getVariable('_1').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_1').value).toBe(2)
  expect(ludolfC.hasVariable('_2')).toBe(true)
  expect(ludolfC.getVariable('_2').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_2').value).toBe(3)
  expect(ludolfC.hasVariable('_3')).toBe(true)
  expect(ludolfC.getVariable('_3').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_3').value).toBe(4)
  expect(ludolfC.hasVariable('_4')).toBe(true)
  expect(ludolfC.getVariable('_4').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_4').value).toBe(5)
  expect(ludolfC.hasVariable('_5')).toBe(true)
  expect(ludolfC.getVariable('_5').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_5').value).toBe(6)
  expect(ludolfC.hasVariable('_6')).toBe(true)
  expect(ludolfC.getVariable('_6').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_6').value).toBe(7)
  expect(ludolfC.hasVariable('_7')).toBe(true)
  expect(ludolfC.getVariable('_7').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_7').value).toBe(8)
  expect(ludolfC.hasVariable('_8')).toBe(true)
  expect(ludolfC.getVariable('_8').type).toBe('NUMBER')
  expect(ludolfC.getVariable('_8').value).toBe(9)
})

test('insertionsort', async () => {
  const result = await ludolfC.execute(fs.readFileSync('./test/insertionsort.lc'))
  expect(ludolfC.hasVariable('arr')).toBe(false)
  expect(result.type).toBe('ARRAY')
  expect(result.value).toHaveLength(9)
  expect(result.value[0].type).toBe('NUMBER')
  expect(result.value[0].value).toBe(1)
  expect(result.value[1].type).toBe('NUMBER')
  expect(result.value[1].value).toBe(2)
  expect(result.value[2].type).toBe('NUMBER')
  expect(result.value[2].value).toBe(3)
  expect(result.value[3].type).toBe('NUMBER')
  expect(result.value[3].value).toBe(4)
  expect(result.value[4].type).toBe('NUMBER')
  expect(result.value[4].value).toBe(5)
  expect(result.value[5].type).toBe('NUMBER')
  expect(result.value[5].value).toBe(6)
  expect(result.value[6].type).toBe('NUMBER')
  expect(result.value[6].value).toBe(7)
  expect(result.value[7].type).toBe('NUMBER')
  expect(result.value[7].value).toBe(8)
  expect(result.value[8].type).toBe('NUMBER')
  expect(result.value[8].value).toBe(9)
})

test('factorial', async () => {
  await ludolfC.execute(fs.readFileSync('./test/factorial.lc'))
  expect(ludolfC.hasVariable('factorial1')).toBe(true)
  expect(ludolfC.hasVariable('factorial2')).toBe(true)
  expect(ludolfC.getVariable('_1_0').value).toBe(1)
  expect(ludolfC.getVariable('_1_1').value).toBe(1)
  expect(ludolfC.getVariable('_1_5').value).toBe(120)
  expect(ludolfC.getVariable('_2_0').value).toBe(1)
  expect(ludolfC.getVariable('_2_1').value).toBe(1)
  expect(ludolfC.getVariable('_2_5').value).toBe(120)
})

test('binarySearch', async () => {
  await ludolfC.execute(fs.readFileSync('./test/binarysearch.lc'))
  expect(ludolfC.hasVariable('binarySearch')).toBe(true)
  expect(ludolfC.getVariable('_10').value).toBe(3)
})

test('source #1', async () => {
  await ludolfC.execute(fs.readFileSync('./test/source1.lc'))
  expect(ludolfC.getVariable('a1').value).toBe(6)
  expect(ludolfC.getVariable('a2').value).toBe(12.345)
  expect(ludolfC.getVariable('a3').value).toBe(14.345)
  expect(ludolfC.getVariable('a4').value).toBe(16.345)
  expect(ludolfC.getVariable('a5').value).toBe(34)
  expect(ludolfC.getVariable('__a__').value).toBe(22.345)
  expect(ludolfC.getVariable('__a__1').value).toBe(36.69)
  expect(ludolfC.getVariable('arr_01__0').value).toBe(256)
  expect(ludolfC.getVariable('arr_07__1_0_0').value).toBe(3)
  expect(ludolfC.getVariable('arr_09__2_0_1').value).toBe(21)
  expect(ludolfC.getVariable('arr_10__1_x').value).toBe(1)
  expect(ludolfC.getVariable('arr_10__1_y').value).toBe(2)
  expect(ludolfC.getVariable('arr_11__0').value).toBe('')
  expect(ludolfC.getVariable('arr_11__1').value).toBe('1')
  expect(ludolfC.getVariable('arr_11__2').value).toBe('abc')
  expect(ludolfC.getVariable('arr_12__0').value).toBe(1.2)
  expect(ludolfC.getVariable('arr_12__1').value).toBe(123.456)
  expect(ludolfC.getVariable('arr_13__0_0_0_a').value).toBe(1)
  expect(ludolfC.getVariable('arr_14__0_0_0_a').value).toBe(2)
  expect(ludolfC.getVariable('arr_14__0_0_0_b').value).toBe(3)
  expect(ludolfC.getVariable('_while1').value).toBe(1)
  expect(ludolfC.getVariable('o1__x').value).toBe(1)
  expect(ludolfC.getVariable('o1__yx').value).toBe(3)
  expect(ludolfC.getVariable('o1__arr_0').value).toBe(1)
  expect(ludolfC.getVariable('o1__arr_1').value).toBe(2)
  expect(ludolfC.getVariable('o2__x').value).toBe(1)
  expect(ludolfC.getVariable('o2__yx').value).toBe(3)
  expect(ludolfC.getVariable('o2__f').value).toBe(125)
  expect(ludolfC.getVariable('_if1').value).toBe(9)
  expect(ludolfC.getVariable('_if2').value).toBe(2)
})

test('object self reference', async () => {
  const res = await ludolfC.execute(`
  math := {
    minus_one: -1,
    plus: (a, b) { a + b },
    minus: (a, b) { plus(a, b * minus_one) }
  }
  math.minus(5, 3)
  `)
  expect(res.type).toBe(lang.Types.NUMBER)
  expect(res.value).toBe(2)
})

test('else-if', async () => {
  const res = await ludolfC.execute(`
  a := 0
  f := (){ a := a + 1; a }
  r := 0
  
  if f() > 1 {
    r := 1
  } else if f() > 1 {
    r := 2
  } else if f() > 1 {
    r := 3
  } else {
    r := 4
  }
  "r" + r + "a" + a
  `)
  expect(res.type).toBe(lang.Types.STRING)
  expect(res.value).toBe('r2a2')
})

test('comments', async () => {
  const res = await ludolfC.execute(`
  // comment 0
  a := 0 // comment 1
  f := (){ a := a + 1; a } // comment 2
  r := 0 // comment 3
  
  if f() > 1 { // comment 4 
    r := 1 // comment 5
  } else if f() > 1 { // comment 6
    r := 2  // comment 7
  } else if f() > 1 { // comment 8
    r := 3 // comment 9
  } else { // comment 10
    r := 4 // comment 11
  } // comment 12
  "r" + r + "a" + a // comment 13
  // comment 14
  `)
  expect(res.type).toBe(lang.Types.STRING)
  expect(res.value).toBe('r2a2')
})