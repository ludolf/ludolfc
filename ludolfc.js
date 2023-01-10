const KEYWORDS = {
    TRUE: ['true', 'pravda', 'wahr'],
    FALSE: ['false', 'nepravda', 'unwahr'],
    IF: ['if', 'pokud', 'falls'],
    ELSE: ['else', 'jinak', 'sonst'],
    WHILE: ['while', 'dokud', 'soweit'],
}

const ERRORS = {
    UNEXPECTED_END: 'UNEXPECTED_END',
    UNEXPEXTED_SYMBOL: 'UNEXPEXTED_SYMBOL',
    EXPEXTED_SYMBOL: 'EXPEXTED_SYMBOL',
    UNREFERENCED_VARIABLE: 'UNREFERENCED_VARIABLE',
    UNEXPEXTED_KEYWORD: 'UNEXPEXTED_KEYWORD',
    INVALID_IDENTIFIER: 'INVALID_IDENTIFIER',
    UNEVEN_OPERATORS: 'UNEVEN_OPERATORS',
    EXPEXTED_FUNCTION: 'EXPEXTED_FUNCTION',
    ATTRIBUTE_NOT_EXISTS: 'ATTRIBUTE_NOT_EXISTS',
    ARRAY_INDEX_NOT_NUMBER: 'ARRAY_INDEX_NOT_NUMBER',
    ARRAY_INDEX_MISSING: 'ARRAY_INDEX_MISSING',
    ARRAY_INDEX_OUT_BOUNDS: 'ARRAY_INDEX_OUT_BOUNDS',
}

const TYPES = {
    NUMBER: 'NUMBER',
    BOOLEAN: 'BOOLEAN',
    STRING: 'STRING',
    ARRAY: 'ARRAY',
    OBJECT: 'OBJECT',
    FUNCTION: 'FUNCTION',
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

class LangObject {
    constructor(value, type = TYPES.OBJECT) {
        this.value = value
        this.type = type
    }
    attribute(name) {
        return this[name] ? this[name] : this.value[name]
    }
    hasAttribute(name) {
        return this[name] || this.value[name]
    }
}

class LangValueObject extends LangObject {
    constructor(value, type) {
        super(value, type)

        this.eq = new LangFunction(x => new LangBoolean(this.value === x.value))
        this.ne = new LangFunction(x => new LangBoolean(this.value !== x.value))
    }
}

class LangNumber extends LangValueObject {
    constructor(value) {
        super(value, TYPES.NUMBER)
        
        this.mult = new LangFunction(x => new LangNumber(this.value * x.value))
        this.div = new LangFunction(x => new LangNumber(this.value / x.value))
        this.mod = new LangFunction(x => new LangNumber(this.value % x.value))
        this.plus = new LangFunction(x => new LangNumber(this.value + x.value))
        this.minus = new LangFunction(x => new LangNumber(this.value - x.value))
        this.lt = new LangFunction(x => new LangBoolean(this.value < x.value))
        this.le = new LangFunction(x => new LangBoolean(this.value <= x.value))
        this.gt = new LangFunction(x => new LangBoolean(this.value > x.value))
        this.ge = new LangFunction(x => new LangBoolean(this.value >= x.value))
        this.neg = new LangFunction(() => new LangNumber(-this.value))
        this.sum = new LangFunction((...x) => new LangNumber(x.reduce((a,c) => a + c.value, this.value)))
    }
}

class LangString extends LangValueObject {
    constructor(value) {
        super(value, TYPES.STRING)

        this.concat = new LangFunction(x => new LangString(this.value + x.value))
        this.length = new LangFunction(() => new LangNumber(this.value.length))
    }
}

class LangBoolean extends LangValueObject {
    constructor(value) {
        super(value, TYPES.BOOLEAN)

        this.and = new LangFunction(x => new LangBoolean(this.value && x.value))
        this.or = new LangFunction(x => new LangBoolean(this.value || x.value))
        this.xor = new LangFunction(x => new LangBoolean(this.value ? !x.value : x.value))
        this.nand = new LangFunction(x => new LangBoolean(!(this.value && x.value)))
        this.neg = new LangFunction(() => new LangBoolean(!this.value))
    }
}

class LangArray extends LangValueObject {
    constructor(value) {
        super(value, TYPES.ARRAY)

        this.concat = new LangFunction(x => new LangArray(this.value.concat(x.value)))
        this.length = new LangFunction(() => new LangNumber(this.value.length))
    }
    element(...index) {
        return index.reduce((a,c) => a.value[Math.ceil(c.value)], this)
    }
}

class LangFunction extends LangObject {
    constructor(fn) {
        super(fn, TYPES.FUNCTION)
    }
    call(...params) {
        return this.value(...params)
    }
}

class Operator {
    constructor(op) {
        this.op = op
        this.precedence = this._precedence()
    }

    uni(a) {
        switch (this.op) {
            case '!': 
            case '-': return a.neg.call()
            default: throw new Error('Invalid uni operator ' + this.op)
        }
    }

    bi(a, b) {
        switch (this.op) {
            case '*': return a.mult.call(b)
            case '/': return a.div.call(b)
            case '%': return a.mod.call(b)
            case '+': return a.plus.call(b)
            case '-': return a.minus.call(b)
            case '<': return a.lt.call(b)
            case '<=': return a.le.call(b)
            case '>': return a.gt.call(b)
            case '>=': return a.ge.call(b)
            case '=': return a.eq.call(b)
            case '!=': return a.ne.call(b)
            case '&': return a.and.call(b)
            case '|': return a.or.call(b)
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
        let inObject = false

        let openArrays = 0  // [
        let openObjects = 0 // {

        for (; !source.finished(); source.move()) {
            const c = source.currentChar()
            
            // consume the whole string to prevent space-ignoring
            if (!inAssignment && !inObject && isStringStarting(c)) {
                let cc = c
                do {
                    token += cc
                    source.move()
                    cc = source.currentChar()
                } while (!source.finished() && !isStringEnding(cc, c))
            }

            if ('[' === c) openArrays++
            if (']' === c) openArrays--
            if ('{' === c) openObjects++
            if ('}' === c) openObjects--
            
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
            if (isStatementSeparator(c) && !openArrays && !openObjects) {
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
                if ('{' === c) {
                    // TODO parse object
                    inObject = true
                }
                else {
                    if (!new RegExp(`^${RE_IDENTIFIER}$`).test(token)) 
                        throw new LangError(ERRORS.INVALID_IDENTIFIER, source.row, source.col, token)
                    
                    const value = this._execExpression(source)
                    this.variables.set(token, value)
                }

                inAssignment = false
                token = ''
            }
            else {
                token += c
            }
        }

        if (inAssignment || inObject) {
            throw new LangError(ERRORS.UNEXPECTED_END, source.row, source.col)
        }

        // statement is an expression
        if (token.length) {
            return this._execExpression(new Source(token))
        }
    }

    _execExpression(source, inGrouping = null) {
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

            // end of the statement
            if (isStatementSeparator(c) || ')' === c || ']' === c || ',' === c) {
                if ((')' === c || ']' === c) && ((!inGrouping && inGrouping !== c) || !members.length)) {
                    throw new LangError(ERRORS.UNEXPEXTED_SYMBOL, source.row, source.col, c)
                }
                // evaluate the list of tokens and operators
                if (members.length) {
                    if (members.length !== biops.length + 1) {
                        throw new LangError(ERRORS.UNEVEN_OPERATORS, source.row, source.col)
                    }
                    return applyOperators(members, biops, uniops)
                }
                source.move()
                continue
            }

            // grouping or a function call
            if ('(' === c) {
                source.move()
                if (rightOperatorExpected()) {    // a function call
                    const params = this._readList(source, ')')
                    consumeSpaces(source)
    
                    if (')' === source.currentChar()) {
                        const idx = members.length - 1
                        if (members[idx].type !== TYPES.FUNCTION)
                            throw new LangError(ERRORS.EXPEXTED_FUNCTION, source.row, source.col, members[members.length - 1])

                        members[idx] = members[idx].call(...params)
                        source.move()
                    } else {
                        throw new LangError(ERRORS.UNEXPEXTED_SYMBOL, source.row, source.col, source.currentChar(), ')')
                    }
                } else {    // grouping
                    expected = ')'
                    members.push(this._execExpression(source, true))
                }
                continue
            }

            // attribute access
            if ('.' === c && rightOperatorExpected()) {
                source.move()
                const attrName = this._readIdentifier(source)
                const idx = members.length - 1
                if (!members[idx].hasAttribute(attrName)) {
                    throw new LangError(ERRORS.ATTRIBUTE_NOT_EXISTS, source.row, source.col, attrName)
                }
                members[idx] = members[idx].attribute(attrName)
                continue
            }

            // arrays
            if ('[' === c) {
                source.move()
                if (rightOperatorExpected()) { // array access
                    const indexes = this._readList(source, ']')
                    consumeSpaces(source)

                    if (!indexes.length) throw new LangError(ERRORS.ARRAY_INDEX_MISSING, source.row, source.col)

                    if (']' === source.currentChar()) {
                        if (indexes.some(x => TYPES.NUMBER !== x.type)) throw new LangError(ERRORS.ARRAY_INDEX_NOT_NUMBER, source.row, source.col)
                        const idx = members.length - 1
                        const value = members[idx].element(...indexes)
                        if (!value) throw new LangError(ERRORS.ARRAY_INDEX_OUT_BOUNDS, source.row, source.col)
                        members[idx] = value
                        source.move()
                    } else {
                        throw new LangError(ERRORS.UNEXPEXTED_SYMBOL, source.row, source.col, source.currentChar(), ']')
                    }
                } else {    // array definition
                    const elements = this._readList(source, ']')
                    consumeSpaces(source)
    
                    if (']' === source.currentChar()) {
                        members.push(new LangArray(elements))
                        source.move()
                    } else {
                        throw new LangError(ERRORS.UNEXPEXTED_SYMBOL, source.row, source.col, source.currentChar(), ']')
                    }
                }
                continue
            }

            // operators
            if (leftOperatorExpected()) {
                if (UNIOPERATORS.includes(c)) {
                    if (!uniops[members.length]) uniops[members.length] = []
                    uniops[members.length].push(new Operator(c))    // index of the operator is the same as of the member to be applied to
                    source.move()
                    continue
                }
            } else if (rightOperatorExpected()) {
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

            const value = this._execMemberExpression(source)
            members.push(value)
        }

        function leftOperatorExpected() {
            return members.length === biops.length
        }

        function rightOperatorExpected() {
            return members.length && members.length === biops.length + 1
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
                    return new LangBoolean(true)
                }
                if (KEYWORDS.FALSE.includes(token.toLowerCase())) {
                    return new LangBoolean(false)
                }
                if (isNumeric(token)) {
                    return new LangNumber(token.includes('.') ? parseFloat(token) : parseInt(token))
                } 
                // variable reference
                if (this.variables.has(token)) {
                    return this.variables.get(token)
                }
                throw new LangError(ERRORS.UNREFERENCED_VARIABLE, source.row, source.col, token)
                
            } else
            if (isStringStarting(c)) {
                source.move()
                return new LangString(this._readString(source, c))
            }

            token += c
        }
    }
    
    _readList(source, groupingCloseChar) {
        consumeSpaces(source)
        if (groupingCloseChar === source.currentChar()) {
            return []
        } else {    // multiple params
            const params = []
            do {
                const value = this._execExpression(source, groupingCloseChar)
                params.push(value)

                consumeSpaces(source)

            } while(',' === source.currentChar())

            return params
        }
    }

    _readString(source, quoting) {
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

    _readIdentifier(source) {
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
    return isSpace(c) || isStatementSeparator(c) || '(' === c || ')' === c || '[' === c || ']' === c || '.' === c || ',' === c
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

function consumeSpaces(source) {
    while (!source.finished() && /\s/.test(source.currentChar())) source.move()
}

function removeElementAt(arr, index) {
    return arr.filter((_, i) => i !== index)
}

module.exports = LudolfC