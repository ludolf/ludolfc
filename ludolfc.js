const ERRORS = {
    UNEXPECTED_END: 'UNEXPECTED_END',
    UNEXPEXTED_SYMBOL: 'UNEXPEXTED_SYMBOL',
    EXPEXTED_SYMBOL: 'EXPEXTED_SYMBOL',
    UNREFERENCED_VARIABLE: 'UNREFERENCED_VARIABLE',
    UNEXPEXTED_KEYWORD: 'UNEXPEXTED_KEYWORD',
    INVALID_IDENTIFIER: 'INVALID_IDENTIFIER',
    UNEVEN_OPERATORS: 'UNEVEN_OPERATORS',
    EXPEXTED_FUNCTION: 'EXPEXTED_FUNCTION',
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

const UNIOPERATORS = ['!', '-']
const BIOPERATORS = ['*', '/', '%', '+', '-', '<', '<=', '>', '>=', '=', '!=', '&', '|']

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

class Operator {
    constructor(op) {
        this.op = op
        this.precedence = this._precedence()
    }

    uni(a) {
        switch (this.op) {
            case '!': return !a
            case '-': return -a
            default: throw new Error('Invalid uni operator ' + this.op)
        }
    }

    bi(a, b) {
        switch (this.op) {
            case '*': return a * b
            case '/': return a / b
            case '%': return a % b
            case '+': return a + b
            case '-': return a - b
            case '<': return a < b
            case '<=': return a <= b
            case '>': return a > b
            case '>=': return a >= b
            case '=': return a === b // a == b
            case '!=': return a !== b // a != b
            case '&': return a && b
            case '|': return a || b
            default: throw new Error('Invalid bi operator ' + this.op)
        }
    }

    _precedence() { // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
        switch (this.op) {
            case '*':
            case '/':
            case '%': return 12
            case '+': 
            case '-': return 11
            case '<': 
            case '<=':
            case '>':
            case '>=': return 9
            case '=': 
            case '!=': return 8
            case '&': return 4
            case '|': return 3
            default: -1
        }
    }

    toString() {
        return this.op
    }
}

class ValueObject {
    constructor(value) {
        this.value = value

        if (typeof value === 'number') {
            this.mult = x => value * x
            this.div = x => value / x
            this.mod = x => value % x
            this.plus = x => value + x
            this.minus = x => value - x
            this.lt = x => value < x
            this.le = x => value <= x
            this.gt = x => value > x
            this.ge = x => value >= x
            this.eq = x => value === x
            this.ne = x => value !== x
        } else
        if (typeof value === 'string') {
            this.concat = x => value + x
            this.length = () => value.length
        } else
        if (typeof value === 'boolean') {
            this.and = x => value && x
            this.or = x => value || x
            this.xor = x => value ? !x : x
            this.nand = x => !(value && x)
            this.neg = () => !value
        }
    }
}

class Source {
    constructor(code) {
        this.code = code + '\n'
        this.pos = 0
        this.row = 0
        this.col = 0
    }

    move(step = 1) {
        this.pos += step
        this.col += step
        if ('\n' === this.currentChar()) {
            this.row++
            this.col = 0
        }
    }

    currentChar() {
        return this.code[this.pos]
    }

    remaining(length = undefined) {
        return this.code.substring(this.pos, this.pos + length)
    }

    next(length = 1) {
        return this.code.substring(Math.min(this.pos + 1, this.code.length - 1), Math.min(this.pos + 1 + length, this.code.length - 1))
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
        return this._execProgram(new Source(code))
    }

    _execProgram(source) {
        let result = null
        while (!source.finished()) {
            const r = this._execStatement(source)
            if (r != null) {
                result = r
            }
        }
        return result
    }

    _execStatement(source) {
        let token = ''
        
        let expecting = null
        let inAssignment = false
        let inArray = false
        let inObject = false

        for (; !source.finished(); source.move()) {
            const c = source.currentChar()
            
            // consume the whole string to prevent space-ignoring
            if (!inAssignment && !inArray && !inObject && isStringStarting(c)) {
                let cc = c
                do {
                    token += cc
                    source.move()
                    cc = source.currentChar()
                } while (!source.finished() && !isStringEnding(cc, c))
            }
            
            // ignore spaces
            if (!expecting && isSpace(c)) continue

            if (expecting && c !== expecting) {
                throw new LangError(ERRORS.EXPEXTED_SYMBOL, source.row, source.col, expecting, c)
            }
            if (expecting === '=' && c === expecting) {
                inAssignment = true
                expecting = null
                continue
            }

            // end of the statement
            if (isStatementSeparator(c)) {
                source.move()
                break
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
                               : typeof value === 'string' ? VARIABLES.STRING
                               : Array.isArray(value) ? VARIABLES.ARRAY
                               : typeof value === 'function' ? VARIABLES.FUNCTION
                               : typeof value === 'object' ? VARIABLES.OBJECT
                               : 'UNKNOWN'
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

        // statement is an expression
        if (token.length) {
            return this._execExpression(new Source(token))
        }
    }

    _execExpression(source, inGrouping = false) {
        const members = []
        const uniops = []
        const biops = []

        let expected = null

        while (!source.finished()) {
            const c = source.currentChar()

            if (isSpace(c)) {
                source.move()
                continue
            }

            if (expected) {
                if (c === expected) {  
                    expected = null
                    source.move()                  
                    continue
                }
                throw new LangError(ERRORS.EXPEXTED_SYMBOL, source.row, source.col, expected)
            }

            if (isStatementSeparator(c) || ')' === c) {
                if (')' === c && (!inGrouping || !members.length)) {
                    throw new LangError(ERRORS.UNEXPEXTED_SYMBOL, source.row, source.col, c)
                }
                // evaluate the list of tokens
                if (members.length) {
                    if (members.length !== biops.length + 1) {
                        throw new LangError(ERRORS.UNEVEN_OPERATORS, source.row, source.col)
                    }
                    return applyOperators(members, biops, uniops)
                }
                source.move()
                continue
            }

            if ('(' === c) {
                expected = ')'
                source.move()
                members.push(this._execExpression(source, true))
                continue
            }

            if ('.' === c && members.length && biops.length === members.length - 1) {
                applyAttributeOp(source, members)
                consumeSpaces(source)

                // apply function call
                if ('(' === source.currentChar()) {
                    source.move()
                    const params = this._readParams(source)  
                    consumeSpaces(source)
    
                    if (')' === source.currentChar()) {
                        if (typeof members[members.length - 1] !== 'function')
                            throw new LangError(ERRORS.EXPEXTED_FUNCTION, source.row, source.col, members[members.length - 1])

                        members[members.length - 1] = members[members.length - 1](...params)
                        source.move()
                    } else {
                        throw new LangError(ERRORS.UNEXPEXTED_SYMBOL, source.row, source.col, source.currentChar(), ')')
                    }
                }
                continue
            }

            if (biops.length === members.length) {
                if (UNIOPERATORS.includes(c)) {
                    if (!uniops[members.length]) uniops[members.length] = []
                    uniops[members.length].push(new Operator(c))    // index of the operator is the same as of the member to be applied to
                    source.move()
                    continue
                }
            } else
            if (biops.length === members.length - 1) {
                const next2 = source.remaining(2)
                if (BIOPERATORS.includes(next2)) {
                    biops.push(new Operator(next2))
                    source.move(2)
                    continue
                }
                if (BIOPERATORS.includes(c)) {
                    biops.push(new Operator(c))
                    source.move()
                    continue
                }
            }

            let mexp = this._execMemberExpression(source)

            members.push(mexp)
        }

        function applyAttributeOp(source, members) {
            source.move()
            const attrName = readIdentifier(source)
            const idx = members.length - 1
            members[idx] = (isPrimitive(members[idx]) ? new ValueObject(members[idx]) : members[idx])[attrName]
        }

        function applyOperators(members, biops, uniops) {
            uniops.forEach((u,i) => members[i] = u.reduceRight((a,c) => c.uni(a), members[i]))

            let result = members[0]
            let opIndex
            while ((opIndex = findNextOp(biops)) > -1) {
                result = biops[opIndex].bi(members[opIndex], members[opIndex + 1])
                biops = removeElementAt(biops, opIndex)
                members = removeElementAt(members, opIndex)
                members[opIndex] = result
            }
            return result

            function findNextOp(ops) {
                let index = -1
                let max = Number.MIN_SAFE_INTEGER
                for (let i = 0; i < ops.length; i++) {
                    const op = ops[i]
                    if (max < op.precedence) {
                        index = i
                        max = op.precedence
                    }
                }
                return index
            }

            function removeElementAt(arr, index) {
                return arr.filter((v,i) => i !== index)
            }
        }

        function readIdentifier(source) {
            let token = ''
            for (; !source.finished(); source.move()) {
                const c = source.currentChar()
                if (isSpace(c)) continue
                if (!new RegExp(`^${RE_IDENTIFIER}$`).test(token + c)) break                
                token += c
            }
            if (token) return token
            throw new LangError(ERRORS.EXPECTED_IDENTIFIER, source.row, source.col)
        }
    }

    _execMemberExpression(source) {
        let token = ''

        for (; !source.finished(); source.move()) {
            const c = source.currentChar()

            // token ends
            if (isExpressionSeparator(c)) {
                if ('.' === c && /[0-9]/.test(source.next())) { // either the operator '.' or a float number
                    token += c
                    continue
                }

                if (KEYWORDS.TRUE.includes(token.toLowerCase())) {
                    return true    
                }
                if (KEYWORDS.FALSE.includes(token.toLowerCase())) {
                    return false    
                }
                if (isNumeric(token)) {
                    return token.includes('.') ? parseFloat(token) : parseInt(token)
                } 
                // variable reference
                if (this.variables.has(token)) {
                    return this.variables.get(token).value
                }
                throw new LangError(ERRORS.UNREFERENCED_VARIABLE, source.row, source.col, token)
                
            } else
            if (isStringStarting(c)) {
                source.move()
                return readSting(source, c)
            }

            token += c
        }

        function readSting(source, quoting) {
            let token = ''
            for (; !source.finished(); source.move()) {
                const c = source.currentChar()

                if (isStringEnding(c, quoting)) {
                    source.move()
                    return token
                }
                token += c
            }
            throw new LangError(ERRORS.UNEXPECTED_END, source.row, source.col)
        }
    }
    
    _readParams(source) {
        consumeSpaces(source)
        if (')' === source.currentChar()) {
            return []
        } else {
            // TODO multiple params
            const p = this._execExpression(source, true)
            return [p]
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

function isSpace(c) {
    return '\n' !== c && /\s+/g.test(c)
}

function isExpressionSeparator(c) {
    return isSpace(c) || isStatementSeparator(c) || '(' === c || ')' === c || '[' === c || ']' === c || '.' === c
        || BIOPERATORS.some(op => op.startsWith(c)) || UNIOPERATORS.some(op => op.startsWith(c))
}

function isStatementSeparator(c) {
    return '\n' === c || ';' === c
}

function isStringStarting(c) {
    return c === '"' || c === '“' || c === '”' || c === "'"
}

function isStringEnding(c, quoting) {
    return quoting === c || (c === '”' && quoting === '“')
}

function isPrimitive(value) {
    return typeof value === 'number' || typeof value === 'string' || typeof value === 'boolean'
}

function consumeSpaces(source) {
    for (; !source.finished() && isSpace(source.currentChar()); source.move()) { }
}

module.exports = LudolfC