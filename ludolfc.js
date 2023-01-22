const Parser = require('./parser')
const Interpret = require('./interpret')

class LudolfC {
    constructor() {
        this.parser = new Parser()
        this.interpret = new Interpret()
    }

    execute(code) {
        const ast = this.parser.parse(code)
        return this.interpret.execute(ast)
    }
}

module.exports = LudolfC