const { LudolfC, lang } = require('./ludolfc')
const ludolfC = new LudolfC({}, { isInterrupted: () => true })

test('interruption user susspend', async () => {
  let thrown = false
  try {
    await ludolfC.execute('1')
  } catch (e) {
    thrown = true
    expect(e.isLangInterruption).toBe(true)
    expect(e.id).toBe(lang.Interruptions.USER_SUSSPEND)
  }
  expect(thrown).toBe(true)
})