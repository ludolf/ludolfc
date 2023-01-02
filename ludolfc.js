const ERRORS = {
    UNEXPECTED_END: 'UNEXPECTED_END',
    UNEXPEXTED_SYMBOL: 'UNEXPEXTED_SYMBOL',
    EXPEXTED_SYMBOL: 'EXPEXTED_SYMBOL',
    UNREFERENCED_VARIABLE: 'UNREFERENCED_VARIABLE',
    UNEXPEXTED_KEYWORD: 'UNEXPEXTED_KEYWORD',
    INVALID_IDENTIFIER: 'INVALID_IDENTIFIER',
}

const STATEMENTS = {
    ASSIGNMENT: 'ASSIGNMENT',
    CONDITION: 'CONDITION',
    LOOP: 'LOOP',
    EXPRESSION: 'EXPRESSION',
}

const VARIABLES = {
    NUMBER: 'NUMBER',
    BOOLEAN: 'BOOLEAN',
    STRING: 'STRING',
    ARRAY: 'ARRAY',
    OBJECT: 'OBJECT',
    FUNCTION: 'FUNCTION',
}

const KEYWORDS = {
    TRUE: ['true', 'pravda', 'wahr'],
    FALSE: ['false', 'nepravda', 'unwahr'],
    IF: ['if', 'pokud', 'falls'],
    ELSE: ['else', 'jinak', 'sonst'],
    WHILE: ['while', 'dokud', 'soweit'],
}

const RE_NATIONAL_CHARS = `ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ`
const RE_IDENTIFIER = `[a-zA-Z_${RE_NATIONAL_CHARS}][a-zA-Z0-9_${RE_NATIONAL_CHARS}]*`
const RE_FUNCTION = `\((\s*${RE_IDENTIFIER}\s*(,\s*${RE_IDENTIFIER}\s*)*)?\)\s*\{.*\}`

class LangError extends Error {
    constructor(id, row, col, arg1, arg2) {
        super(`Error ${id} at ${row}:${col}`)
        this.id = id
        this.arg1 = arg1
        this.arg2 = arg2
    }
}

class Statement {
    constructor(type) {
        this.type = type
    }
}

class Variable {
    constructor(type, value) {
        this.type = type
        this.value = value
    }
}

class Source {
    constructor(code) {
        this.code = code + '\n'
        this.pos = 0
        this.row = 0
        this.col = 0
    }

    move() {
        this.pos++
        this.col++
        if ('\n' === this.currentChar()) {
            this.row++
            this.col = 0
        }
    }

    currentChar() {
        return this.code[this.pos]
    }

    remaining() {
        return this.code.substring(this.pos)
    }

    finished() {
        return this.pos >= this.code.length
    }
}

class LudolfC {
    constructor() {
        this.variables = new Map()
    }

    exec(code) {
        this.variables.clear()
        this._execProgram(new Source(code))
    }

    _execProgram(source) {
        while (!source.finished()) {
            this._execStatement(source)
        }
    }

    _execStatement(source) {
        let token = ''
        
        let expecting = null
        let inAssignment = false
        let inArray = false
        let inObject = false

        for (; !source.finished(); source.move()) {
            const c = source.currentChar()
            
            if (!expecting && /\s+/g.test(c)) continue

            if (expecting && c !== expecting) {
                throw new LangError(ERRORS.EXPEXTED_SYMBOL, source.row, source.col, expecting, c)
            }
            if (expecting === '=' && c === expecting) {
                inAssignment = true
                expecting = null
                continue
            }

            if (':' === c) {
                if (!(token.length)) throw new LangError(ERRORS.UNEXPEXTED_SYMBOL, source.row, source.col, c)
                if (isKeyword(token)) throw new LangError(ERRORS.UNEXPEXTED_KEYWORD, source.row, source.col, c)
                expecting = '='
            } else
            if (inAssignment) {
                if (new RegExp(`^${RE_FUNCTION}`).test(source.remaining())) {
                    this.variables.set(token, 
                        new Variable(VARIABLES.FUNCTION, this._parseFunction(source)))
                } else
                if ('[' === c) {
                    // TODO parse array
                    inArray = true
                } else 
                if ('{' === c) {
                    // TODO parse object
                    inObject = true
                }
                else {
                    if (!new RegExp(`^${RE_IDENTIFIER}$`).test(token)) 
                        throw new LangError(ERRORS.INVALID_IDENTIFIER, source.row, source.col, token)
                    
                    const value = this._execExpression(source)
                    const type = typeof value === 'number' ? VARIABLES.NUMBER
                               : typeof value === 'boolean' ? VARIABLES.BOOLEAN
                               : VARIABLES.STRING
                    this.variables.set(token, new Variable(type, value))
                }

                inAssignment = false
                token = ''
            }
            else {
                token += c
            }
        }

        if (inAssignment || inArray || inObject) {
            throw new LangError(ERRORS.UNEXPECTED_END, source.row, source.col)
        }
    }

    _execExpression(source) {
        const tokens = []

        while (!source.finished()) {
            const c = source.currentChar()

            if ('\n' === c) {
                // TODO evaluate the list of tokens
                if (tokens.length) {
                    return tokens[0]
                }
            }

            if (/\s+/g.test(c)) {
                source.move()
                continue
            }

            tokens.push(this._execMemberExpression(source))
        }
    }

    _execMemberExpression(source) {
        let token = ''

        let quoting = null
        let inString = false

        for (; !source.finished(); source.move()) {
            const c = source.currentChar()

            // token ends
            if (!inString && /\s+/g.test(c)) {
                if (KEYWORDS.TRUE.includes(token.toLowerCase())) {
                    return true
    
                } else 
                if (KEYWORDS.FALSE.includes(token.toLowerCase())) {
                    return false
    
                } else
                if (isNumeric(token)) {
                    return token.includes('.') ? parseFloat(token) : parseInt(token)
                } 
                else { // variable reference
                    if (this.variables.has(token)) {
                        return this.variables.get(token).value
                    }
                    throw new LangError(ERRORS.UNREFERENCED_VARIABLE, source.row, source.col, token)
                }
            }

            if (inString) {
                if (quoting === c || (c === '”' && quoting === '“')) {
                    inString = false
                    quoting = null
                    source.move()
                    return token
                }

            } else
            if (c === '"' || c === '“' || c === '”' || c === "'") {
                inString = true
                quoting = c
                continue
            }

            token += c
        }
    }

    _execCondition(source) {
        
    }

    _execLoop(source) {

    }

    _execCall(source) {

    }
}

function isNumeric(str) {
    return !isNaN(str) && !isNaN(parseFloat(str))
}

function isKeyword(str) {
    str = str.toLowerCase()
    return Object.values(KEYWORDS).some(kw => kw.includes(str))
}

module.exports = LudolfC