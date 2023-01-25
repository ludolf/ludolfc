const Parser = require('./parser')
const Interpret = require('./interpret')

class LudolfC {
    constructor(imports = {}) {
        this.parser = new Parser()
        this.interpret = new Interpret(imports)
    }

    execute(code) {
        const ast = this.parser.parse(code)
        return this.interpret.execute(ast)
    }

    hasVariable(name) {
        return this.interpret.hasVariable(name)
    }

    getVariable(name) {
        return this.interpret.getVariable(name)
    }
}

module.exports = LudolfC