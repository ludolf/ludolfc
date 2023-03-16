const lang = require('./lang')
const Parser = require('./parser')
const Interpret = require('./interpret')

class LudolfC {
    /**
     * @param {object} imports 
     * @param {isInterrupted: () => boolean} controls 
     */
    constructor(imports = {}, controls = {}) {
        this.parser = new Parser()
        this.interpret = new Interpret(imports, controls)
    }

    /**
     * Execute LudolfC code.
     * @param {string} code 
     * @returns result of the execution
     */
    async execute(code) {
        try {
            const ast = this.parser.parse(code)
            return await this.interpret.execute(ast)

        } catch (e) {
            if (e.isLangError && (e.position || e.position === 0)) {
                const {line, col} = lineAndCol(this.parser.source.code, e.position)
                e.line = line
                e.col = col
            }
            throw e
        }
    }

    hasVariable(name) {
        return this.interpret.variables.hasVariable(name)
    }

    getVariable(name) {
        return this.interpret.variables.getVariable(name)
    }
}

function lineAndCol(code, position) {
    let line = 1    // starting from 1
    let col = 1
    for (let i = 0; i < code.length && i <= position; i++) {
        col++
        if ('\n' === code.charAt(i)) {
            line++
            col = 1
        }
    }
    col = Math.max(1, col - 1)
    return {line, col}
}

module.exports = { LudolfC, lang }
