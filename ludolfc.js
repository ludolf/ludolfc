const Keywords = {
    TRUE: ['true', 'pravda', 'wahr'],
    FALSE: ['false', 'nepravda', 'unwahr'],
    IF: ['if', 'pokud', 'falls'],
    ELSE: ['else', 'jinak', 'sonst'],
    WHILE: ['while', 'dokud', 'soweit'],
}

const Errors = {
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
    FUNC_ARGUMENTS_MISHMASH: 'FUNC_ARGUMENTS_MISHMASH',
}

const Types = {
    NUMBER: 'NUMBER',
    BOOLEAN: 'BOOLEAN',
    STRING: 'STRING',
    ARRAY: 'ARRAY',
    OBJECT: 'OBJECT',
    FUNCTION: 'FUNCTION',
    VOID: 'VOID',
}

const UniOperators = ['!', '-']
const BiOperators = ['*', '/', '%', '+', '-', '<', '<=', '>', '>=', '=', '!=', '&', '|']

const RE_NATIONAL_CHARS = `ěščřžýáíéúůüöäñĚŠČŘŽÝÁÍÉÚŮÜÖÄÑ`
const RE_IDENTIFIER = `[a-zA-Z_${RE_NATIONAL_CHARS}][a-zA-Z0-9_${RE_NATIONAL_CHARS}]*`
const RE_FUNCTION = `\\((\\s*(${RE_IDENTIFIER})\\s*(,\\s*(${RE_IDENTIFIER}))*)?\\s*\\)\\s*\{(.|\\s)*\\}`

class LangError extends Error {
    constructor(id, row, col, arg1, arg2) {
        super(`Error ${id} at ${row}:${col}`)
        this.id = id
        this.arg1 = arg1
        this.arg2 = arg2
    }
}

class LangObject {
    constructor(value, type = Types.OBJECT) {
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

        this.eq = new LangNativeFunction(x => new LangBoolean(this.value === x.value))
        this.ne = new LangNativeFunction(x => new LangBoolean(this.value !== x.value))
    }
}

class LangNumber extends LangValueObject {
    constructor(value) {
        super(value, Types.NUMBER)
        
        this.mult = new LangNativeFunction(x => new LangNumber(this.value * x.value))
        this.div = new LangNativeFunction(x => new LangNumber(this.value / x.value))
        this.mod = new LangNativeFunction(x => new LangNumber(this.value % x.value))
        this.plus = new LangNativeFunction(x => new LangNumber(this.value + x.value))
        this.minus = new LangNativeFunction(x => new LangNumber(this.value - x.value))
        this.lt = new LangNativeFunction(x => new LangBoolean(this.value < x.value))
        this.le = new LangNativeFunction(x => new LangBoolean(this.value <= x.value))
        this.gt = new LangNativeFunction(x => new LangBoolean(this.value > x.value))
        this.ge = new LangNativeFunction(x => new LangBoolean(this.value >= x.value))
        this.neg = new LangNativeFunction(() => new LangNumber(-this.value))
        this.sum = new LangNativeFunction((...x) => new LangNumber(x.reduce((a,c) => a + c.value, this.value)))
    }
}

class LangString extends LangValueObject {
    constructor(value) {
        super(value, Types.STRING)

        this.concat = new LangNativeFunction(x => new LangString(this.value + x.value))
        this.length = new LangNativeFunction(() => new LangNumber(this.value.length))
    }
}

class LangBoolean extends LangValueObject {
    constructor(value) {
        super(value, Types.BOOLEAN)

        this.and = new LangNativeFunction(x => new LangBoolean(this.value && x.value))
        this.or = new LangNativeFunction(x => new LangBoolean(this.value || x.value))
        this.xor = new LangNativeFunction(x => new LangBoolean(this.value ? !x.value : x.value))
        this.nand = new LangNativeFunction(x => new LangBoolean(!(this.value && x.value)))
        this.neg = new LangNativeFunction(() => new LangBoolean(!this.value))
    }
}

class LangArray extends LangValueObject {
    constructor(value) {
        super(value, Types.ARRAY)

        this.concat = new LangNativeFunction(x => new LangArray(this.value.concat(x.value)))
        this.length = new LangNativeFunction(() => new LangNumber(this.value.length))
    }
    element(...index) {
        return index.reduce((a,c) => a.value[Math.ceil(c.value)], this)
    }
}

class LangFunction extends LangObject {
    constructor(body, args, parent) {
        super(body, Types.FUNCTION)
        this.args = args
        this.parent = parent
    }
    call(interpret, ...params) {
        if ((!params && this.args) || params.length !== this.args.length) {
            throw new LangError(Errors.FUNC_ARGUMENTS_MISHMASH, interpret.source.row, interpret.source.col)
        }
        // TODO add this.parent as $ into variables
        // cache variables
        const cache = {}
        let i = 0
        for (let arg of this.args) {
            if (interpret.variables.has(arg)) {
                cache[arg] = interpret.variables.get(arg)
            }
            interpret.variables.set(arg, params[i++])
        }
        
        try {
            return interpret._execProgram(new Source(this.value))

        } finally {  // clean up variables
            interpret.variables.delete('$')
            for (let arg of this.args) {
                if (cache[arg]) {
                    interpret.variables.set(arg, cache[arg])
                } else {
                    interpret.variables.delete(arg)
                }
            }
        }
    }
}

class LangNativeFunction extends LangObject {
    constructor(func) {
        super(func, Types.FUNCTION)
        this.native = true
    }
    call(_, ...params) {
        return this.value(...params)
    }
}

class LangVoid extends LangValueObject {
    constructor() {
        super(null, Types.VOID)

        this.eq = new LangNativeFunction(x => new LangBoolean(false))
        this.ne = new LangNativeFunction(x => new LangBoolean(false))
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
            case '*': return a.mult.call(null, b)
            case '/': return a.div.call(null, b)
            case '%': return a.mod.call(null, b)
            case '+': return a.plus.call(null, b)
            case '-': return a.minus.call(null, b)
            case '<': return a.lt.call(null, b)
            case '<=': return a.le.call(null, b)
            case '>': return a.gt.call(null, b)
            case '>=': return a.ge.call(null, b)
            case '=': return a.eq.call(null, b)
            case '!=': return a.ne.call(null, b)
            case '&': return a.and.call(null, b)
            case '|': return a.or.call(null, b)
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
        return this.code.substring(this.pos, Math.min(length ? this.pos + length : this.code.length - 1, this.code.length - 1))
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

        const openDefinitions = {
            arrays: 0,  // [
            objects: 0, // {
        }

        for (; !source.finished(); source.move()) {
            const c = source.currentChar()
            
            // consume the whole string to prevent space-ignoring
            if (!inAssignment && isStringStarting(c)) {
                let cc = c
                do {
                    token += cc
                    source.move()
                    cc = source.currentChar()
                } while (!source.finished() && !isStringEnding(cc, c))
            }
            
            // ignore spaces (except space between numbers)
            if (!expecting && isSpace(c) && !/^[0-9]+$/.test(token)) continue

            if (expecting && c !== expecting) {
                throw new LangError(Errors.EXPEXTED_SYMBOL, source.row, source.col, expecting, c)
            }
            if (expecting === '=' && c === expecting) {
                inAssignment = true
                expecting = null
                continue
            }

            if ('[' === c) openDefinitions.arrays++
            if (']' === c) openDefinitions.arrays--
            if ('{' === c) openDefinitions.objects++
            if ('}' === c) openDefinitions.objects--

            // end of the statement
            if (isStatementSeparator(c) && !openDefinitions.arrays && !openDefinitions.objects) {
                source.move()
                break
            }

            if (':' === c && !openDefinitions.objects) {    // assignment starting
                if (!(token.length)) throw new LangError(Errors.UNEXPEXTED_SYMBOL, source.row, source.col, c)
                if (isKeyword(token)) throw new LangError(Errors.UNEXPEXTED_KEYWORD, source.row, source.col, c)
                expecting = '='
            } else
            if (inAssignment) {  // variable assignment                
                if (!new RegExp(`^${RE_IDENTIFIER}$`).test(token)) 
                    throw new LangError(Errors.INVALID_IDENTIFIER, source.row, source.col, token)
                
                const value = this._execExpression(source, openDefinitions)
                this.variables.set(token, value)

                inAssignment = false
                token = ''
            }
            else {
                token += c
            }
        }

        if (inAssignment) {
            throw new LangError(Errors.UNEXPECTED_END, source.row, source.col)
        }

        // statement is an expression
        if (token.length) {
            return this._execExpression(new Source(token), {})
        }
    }

    _execExpression(source, openDefinitions, inGrouping = null) {
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
                throw new LangError(Errors.EXPEXTED_SYMBOL, source.row, source.col, expected)
            }

            // end of the statement
            if (isStatementSeparator(c) || ')' === c || ']' === c || '}' === c || ',' === c) {
                if ((')' === c || ']' === c || '}' === c) && ((!inGrouping && inGrouping !== c) || !members.length)) {
                    throw new LangError(Errors.UNEXPEXTED_SYMBOL, source.row, source.col, c)
                }
                // evaluate the list of tokens and operators
                if (members.length) {
                    if (members.length !== biops.length + 1) {
                        throw new LangError(Errors.UNEVEN_OPERATORS, source.row, source.col)
                    }
                    return applyOperators(members, biops, uniops)
                }
                source.move()
                continue
            }

            // function defition
            if (new RegExp(`^${RE_FUNCTION}`).test(source.remaining())) {
                if (members.length || uniops.length || biops.length) {
                    throw new LangError(Errors.UNEXPEXTED_SYMBOL, source.row, source.col, c)
                }
                return this._parseFunction(source)
            }

            // object definition
            if ('{' === c) {
                if (!leftOperatorExpected()) {
                    throw new LangError(Errors.UNEXPEXTED_SYMBOL, source.row, source.col, c)
                }
                source.move()
                const attributes = this._readAttributes(source, ')')

                consumeSpaces(source)
                if ('}' === source.currentChar()) {
                    members.push(new LangObject(attributes))
                    source.move()
                    openDefinitions.objects--
                    continue
                }
                throw new LangError(Errors.UNEXPEXTED_SYMBOL, source.row, source.col, source.currentChar(), '}')
            }

            // grouping or a function call
            if ('(' === c) {
                source.move()
                if (rightOperatorExpected()) {    // a function call
                    const params = this._readList(source, ')')
                    consumeSpaces(source)
    
                    if (')' === source.currentChar()) {
                        const idx = members.length - 1
                        if (members[idx].type !== Types.FUNCTION)
                            throw new LangError(Errors.EXPEXTED_FUNCTION, source.row, source.col, members[members.length - 1])

                        members[idx] = members[idx].call(this, ...params)
                        source.move()
                    } else {
                        throw new LangError(Errors.UNEXPEXTED_SYMBOL, source.row, source.col, source.currentChar(), ')')
                    }
                } else {    // grouping
                    expected = ')'
                    members.push(this._execExpression(source, openDefinitions, true))
                }
                continue
            }

            // attribute access
            if ('.' === c && rightOperatorExpected()) {
                source.move()
                const attrName = this._readIdentifier(source)
                const idx = members.length - 1
                if (!members[idx].hasAttribute(attrName)) {
                    throw new LangError(Errors.ATTRIBUTE_NOT_EXISTS, source.row, source.col, attrName)
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

                    if (!indexes.length) throw new LangError(Errors.ARRAY_INDEX_MISSING, source.row, source.col)

                    if (']' === source.currentChar()) {
                        if (indexes.some(x => Types.NUMBER !== x.type)) throw new LangError(Errors.ARRAY_INDEX_NOT_NUMBER, source.row, source.col)
                        const idx = members.length - 1
                        const value = members[idx].element(...indexes)
                        if (!value) throw new LangError(Errors.ARRAY_INDEX_OUT_BOUNDS, source.row, source.col)
                        members[idx] = value
                        source.move()
                    } else {
                        throw new LangError(Errors.UNEXPEXTED_SYMBOL, source.row, source.col, source.currentChar(), ']')
                    }
                } else {    // array definition
                    const elements = this._readList(source, ']')
                    consumeSpaces(source)
    
                    if (']' === source.currentChar()) {
                        members.push(new LangArray(elements))
                        source.move()
                        openDefinitions.arrays--
                    } else {
                        throw new LangError(Errors.UNEXPEXTED_SYMBOL, source.row, source.col, source.currentChar(), ']')
                    }
                }
                continue
            }

            // operators
            if (leftOperatorExpected()) {
                if (UniOperators.includes(c)) {
                    if (!uniops[members.length]) uniops[members.length] = []
                    uniops[members.length].push(new Operator(c))    // index of the operator is the same as of the member to be applied to
                    source.move()
                    continue
                }
            } else if (rightOperatorExpected()) {
                const next2 = source.remaining(2)
                if (BiOperators.includes(next2)) {
                    biops.push(new Operator(next2))
                    source.move(2)
                    continue
                }
                if (BiOperators.includes(c)) {
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

                if (Keywords.TRUE.includes(token.toLowerCase())) {
                    return new LangBoolean(true)
                }
                if (Keywords.FALSE.includes(token.toLowerCase())) {
                    return new LangBoolean(false)
                }
                if (isNumeric(token)) {
                    return new LangNumber(token.includes('.') ? parseFloat(token) : parseInt(token))
                } 
                // variable reference
                if (this.variables.has(token)) {
                    return this.variables.get(token)
                }
                throw new LangError(Errors.UNREFERENCED_VARIABLE, source.row, source.col, token)
                
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
                const value = this._execExpression(source, {}, groupingCloseChar)
                params.push(value)

                consumeSpaces(source)

            } while(',' === source.currentChar() && !source.finished())

            return params
        }
    }
    
    _readAttributes(source) {
        consumeSpaces(source)
        if ('}' === source.currentChar()) {
            return {}
        } else {    // multiple attributes
            const attributes = {}
            let first = true
            do {
                if (!first) {
                    source.move()
                }
                first = false

                const name = this._readIdentifier(source)
                consumeSpaces(source)

                if (':' !== source.currentChar()) {
                    throw new LangError(Errors.EXPEXTED_SYMBOL, source.row, source.col, ':', source.currentChar())
                }
                source.move()

                const value = this._execExpression(source, {}, '}')
                attributes[name] = value

                consumeSpaces(source)

            } while(',' === source.currentChar() && !source.finished())

            return attributes
        }
    }

    _parseFunction(source) {
        source.move()
        const args = this._readArguments(source)
        source.move()
        const body = this._parseFuncBody(source)
        source.move()

        return /^\s*$/.test(body) ? new LangNativeFunction(() => new LangVoid()) : new LangFunction(body, args)
    }
    
    _readArguments(source) {
        consumeSpaces(source)
        if (')' === source.currentChar()) {
            return []
        } else {    // multiple arguments
            const args = []
            let first = true
            do {
                if (!first) {
                    source.move()
                }
                first = false

                const name = this._readIdentifier(source)
                args.push(name)

                consumeSpaces(source)

            } while(',' === source.currentChar() && !source.finished())

            consumeSpaces(source)

            if (')' !== source.currentChar()) {
                throw new LangError(Errors.EXPEXTED_SYMBOL, source.row, source.col, ')', source.currentChar())
            }
            return args
        }
    }

    _parseFuncBody(source) {
        consumeSpaces(source)

        if ('{' !== source.currentChar()) {
            throw new LangError(Errors.EXPEXTED_SYMBOL, source.row, source.col, '{', source.currentChar())
        }

        source.move()
        consumeSpaces(source)

        if ('}' === source.currentChar()) {
            return ''
        } else {            
            let body = ''
            let openQuotings = 0
            do {
                const c = source.currentChar()
                body += c
                source.move()

                if ('{' === c) openQuotings++
                if ('}' === c) openQuotings--

            } while((openQuotings || '}' !== source.currentChar()) && !source.finished())

            if ('}' !== source.currentChar()) {
                throw new LangError(Errors.EXPEXTED_SYMBOL, source.row, source.col, '}', source.currentChar())
            }
            return body
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
        throw new LangError(Errors.UNEXPECTED_END, source.row, source.col)
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
        throw new LangError(Errors.EXPECTED_IDENTIFIER, source.row, source.col)
    }
}

function isNumeric(str) {
    return !isNaN(str) && !isNaN(parseFloat(str))
}

function isKeyword(str) {
    str = str.toLowerCase()
    return Object.values(Keywords).some(kw => kw.includes(str))
}

function isSpace(c) {
    return '\n' !== c && /\s+/g.test(c)
}

function isExpressionSeparator(c) {
    return isSpace(c) || isStatementSeparator(c) 
        || '(' === c || ')' === c || '[' === c || ']' === c || '{' === c || '}' === c 
        || '.' === c || ',' === c
        || BiOperators.some(op => op.startsWith(c)) || UniOperators.some(op => op.startsWith(c))
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