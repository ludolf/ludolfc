const { 
    Errors,
    Keywords,
    WhileKeywords,
    IfKeywords,
    ElseKeywords,
    Block,
    Assignment,
    While,
    If,
    Expression,
    Variable,
    UniOperator,
    BiOperator,
    ArrayAccess,
    ObjectAccess,
    FunctionCall,
    VarReference,
    ParseError: LangParseError,
    Object: LangObject,
    Number: LangNumber,
    String: LangString,
    Boolean: LangBoolean,
    Array: LangArray,
    Function: LangFunction,
    Void: LangVoid } = require('./lang')

const UniOperators = ['!', '-']
const BiOperators = ['*', '/', '%', '+', '-', '<', '<=', '>', '>=', '=', '!=', '&', '|']

const RE_NATIONAL_CHARS = `ěščřžťďýáíéúůüöäñĚŠČŘŽŤĎÝÁÍÉÚŮÜÖÄÑß`
const RE_IDENTIFIER = `[a-zA-Z_${RE_NATIONAL_CHARS}][a-zA-Z0-9_${RE_NATIONAL_CHARS}]*`
const RE_FUNCTION = `\\((\\s*(${RE_IDENTIFIER})\\s*(,\\s*(${RE_IDENTIFIER}))*)?\\s*\\)\\s*\{(.|\\s)*\\}`

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

class Parser {
    constructor() {    
        this.steps = 0
        this.maxSteps = 1000000 // to prevent infinite loops
    }

    parse(code) {
        this.steps = 0
        return this.parseBlock(new Source(code))
    }

    parseBlock(source) {
        const statements = []
        while (!source.finished()) {
            const stm = this.parseStatement(source)
            if (stm) statements.push(stm)
        }
        return new Block(statements)
    }

    parseStatement(source) {
        this._stepper()

        let token = ''
        
        let expecting = null
        let inAssignment = false

        const openDefinitions = {
            arrays: 0,  // [
            objects: 0, // {
        }

        for (; !source.finished(); source.move()) {
            this._stepper()

            const c = source.currentChar()

            // comment
            if (isComment(source.remaining())) {
                consumeUntil(source, '\\n')
                continue
            }
            
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
            if (!expecting && isSpace(c) && isSpace(token.charAt(token.length - 1))) continue

            if (expecting && c !== expecting) {
                throw new LangParseError(Errors.EXPECTED_SYMBOL, expecting, c)
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

            // while
            if (isWhileDef(source.remaining())) {
                if (token.length) {
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, token)
                }
                consumeSpaces(source)
                consumeUntil(source, '\\s')
                const def = this.parseWhile(source)
                consumeSpaces(source, true)
                if (!isStatementSeparator(source.currentChar())) throw new LangParseError(Errors.EXPEXTED_STATEMENT_END)
                return def
            }

            // if
            if (isIfDef(source.remaining())) {
                if (token.length) {
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, token)
                }
                consumeSpaces(source)
                consumeUntil(source, '\\s')
                const def = this.parseIf(source)
                consumeSpaces(source, true)
                if (isElseDef(source.remaining())) {
                    consumeSpaces(source)
                    consumeUntil(source, '\\s')
                    def.elseBody = this.parseBody(source)
                    consumeSpaces(source, true)
                }
                if (!isStatementSeparator(source.currentChar())) throw new LangParseError(Errors.EXPEXTED_STATEMENT_END)
                if (!def.elseBody) {
                    consumeSpaces(source)
                    if (isElseDef(source.remaining())) {
                        consumeSpaces(source)
                        consumeUntil(source, '\\s')
                        def.elseBody = this.parseBody(source)
                        consumeSpaces(source, true)
                        if (!isStatementSeparator(source.currentChar())) throw new LangParseError(Errors.EXPEXTED_STATEMENT_END)
                    }
                }
                return def
            }

            if (':' === c && !openDefinitions.objects) {    // assignment starting
                if (!(token.trim().length)) throw new LangParseError(Errors.UNEXPECTED_SYMBOL, c)
                if (isKeyword(token.trim())) throw new LangParseError(Errors.UNEXPEXTED_KEYWORD, c)
                expecting = '='
            } else
            if (inAssignment) {  // variable assignment                
                const value = this.parseExpression(source, openDefinitions)
                token = token.trim()
                if (isIdentifier(token)) {
                    const variable = new Variable(token)
                    const assignment = new Assignment(variable, value)
                    return assignment
                } else {
                    const exp = this.parseExpression(new Source(token), {})
                    if (!exp || exp.parts.some(p => p.isOperator && !p.isAccess))
                        throw new LangParseError(Errors.INVALID_IDENTIFIER, token)
                    const assignment = new Assignment(exp, value)
                    return assignment
                }
            }
            else {
                token += c
            }
        }

        // statement is an expression
        if (token.length) {
            const exp = this.parseExpression(new Source(token), {})
            return exp
        }
    }

    parseExpression(source, openDefinitions, inGrouping = null) {
        this._stepper()

        const parts = []

        while (!source.finished()) {
            this._stepper()

            const c = source.currentChar()

            // spaces
            if (isSpace(c)) {
                source.move()
                continue
            }

            // end of the statement
            if (isStatementSeparator(c) || ')' === c || ']' === c || '}' === c || ',' === c) {
                if ((')' === c || ']' === c || '}' === c) && ((!inGrouping && inGrouping !== c) || !parts.length)) {
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, c)
                }
                // return the list of tokens and operators
                if (parts.length) {
                    if (parts[parts.length - 1].isOperator && !parts[parts.length - 1].isAccess) {
                        throw new LangParseError(Errors.UNEVEN_OPERATORS)
                    }
                    return new Expression(parts)
                }
                source.move()
                continue
            }

            // function defition
            if (isFunctionDef(source.remaining())) {
                if (parts.length && !parts[parts.length - 1].isOperator) {
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, c)
                }
                const fn = this.parseFunction(source)
                parts.push(fn)
                continue
            }

            // object definition
            if ('{' === c) {
                if (!leftOperatorExpected()) {
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, c)
                }
                source.move()
                const attributes = this.readAttributes(source, ')')

                consumeSpaces(source)
                if ('}' === source.currentChar()) {
                    const obj = new LangObject(attributes)
                    // set the self reference
                    for (let attr of Object.values(attributes)) {
                        if (attr.isObject) {
                            attr.parent = obj
                        }
                    }
                    parts.push(obj)                    
                    source.move()
                    openDefinitions.objects--
                    continue
                }
                throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.currentChar(), '}')
            }

            // grouping or a function call
            if ('(' === c) {
                source.move()
                if (rightOperatorExpected()) {    // a function call
                    const params = this.readList(source, ')')
                    consumeSpaces(source)
    
                    if (')' === source.currentChar()) {
                        var call = new FunctionCall(params)
                        parts.push(call)
                        source.move()
                    } else {
                        throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.currentChar(), ')')
                    }
                } else {    // grouping
                    const exp = this.parseExpression(source, openDefinitions, true)
                    parts.push(exp)
                    consumeSpaces(source)
                    if (')' !== source.currentChar()) throw new LangParseError(Errors.EXPECTED_SYMBOL, ')')
                    source.move()
                }
                continue
            }

            // object attribute access
            if ('.' === c && rightOperatorExpected()) {
                source.move()
                const attrName = this.readIdentifier(source)               
                parts.push(new ObjectAccess(attrName))
                continue
            }

            // array
            if ('[' === c) {
                source.move()
                if (rightOperatorExpected()) { // array access
                    const indexes = this.readList(source, ']')
                    consumeSpaces(source)

                    if (!indexes.length) throw new LangParseError(Errors.ARRAY_INDEX_MISSING)

                    if (']' === source.currentChar()) {
                        if (indexes.some(i => !i.isExpression || !i.parts.length)) throw new LangParseError(Errors.ARRAY_INDEX_NOT_NUMBER)
                        source.move()
                        parts.push(new ArrayAccess(indexes))
                    } else {
                        throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.currentChar(), ']')
                    }
                } else {    // array definition
                    const elements = this.readList(source, ']')
                    consumeSpaces(source)
    
                    if (']' === source.currentChar()) {
                        parts.push(new LangArray(elements))
                        source.move()
                        openDefinitions.arrays--
                    } else {
                        throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.currentChar(), ']')
                    }
                }
                continue
            }

            // operators
            if (leftOperatorExpected()) {
                if (UniOperators.includes(c)) {
                    parts.push(new UniOperator(c))
                    source.move()
                    if (isSpace(source.currentChar())) throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.currentChar())
                    continue
                }
            } else 
            if (rightOperatorExpected()) {
                const next2 = source.remaining(2)
                if (BiOperators.includes(next2)) {
                    parts.push(new BiOperator(next2))
                    source.move(2)
                    continue
                }
                if (BiOperators.includes(c)) {
                    parts.push(new BiOperator(c))
                    source.move()
                    continue
                }
            }

            if (parts.length && (!parts[parts.length - 1].isOperator || parts[parts.length - 1].isAccess)) 
                throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.currentChar())

            const exp = this.parseMemberExpression(source)
            parts.push(exp)
        }

        function leftOperatorExpected() {
            return !parts.length || (parts[parts.length - 1].isOperator && !parts[parts.length - 1].isAccess)
        }

        function rightOperatorExpected() {
            return parts.length && (!parts[parts.length - 1].isOperator || parts[parts.length - 1].isAccess)
        }
    }

    parseMemberExpression(source) {
        this._stepper()

        let token = ''

        for (; !source.finished(); source.move()) {
            this._stepper()
            
            const c = source.currentChar()

            // token ends
            if (isExpressionSeparator(c)) {
                if ('.' === c && /^(0|([1-9][0-9]*))$/.test(token) && /[0-9]/.test(source.next())) { // float number
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
                if (isIdentifier(token) || '$' === token) {
                    return new VarReference(token)
                }
                throw new LangParseError(Errors.UNREFERENCED_VARIABLE, token)                
            }

            if (isStringStarting(c)) {
                source.move()
                return new LangString(this.readString(source, c))
            }

            token += c
        }
    }

    readList(source, groupingCloseChar) {
        consumeSpaces(source)
        if (groupingCloseChar === source.currentChar()) {
            return []
        } else {    // multiple params
            const params = []
            do {
                const value = this.parseExpression(source, {}, groupingCloseChar)
                params.push(value)

                consumeSpaces(source)

            } while(',' === source.currentChar() && !source.finished())

            return params
        }
    }

    readAttributes(source) {
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
                consumeSpaces(source)

                const name = this.readIdentifier(source)
                if (attributes[name]) {
                    throw new LangParseError(Errors.ATTRIBUTE_ALREADY_EXISTS, name)
                }
                consumeSpaces(source)

                if (':' !== source.currentChar()) {
                    throw new LangParseError(Errors.EXPECTED_SYMBOL, ':', source.currentChar())
                }
                source.move()

                const value = this.parseExpression(source, {}, '}')
                attributes[name] = value

                consumeSpaces(source)

            } while(',' === source.currentChar() && !source.finished())

            return attributes
        }
    }

    parseWhile(source) {
        const condCode = this.readUntilBodyOpens(source)
        const cond = this.parseExpression(new Source(condCode), {}, null)
        const body = this.parseBody(source)
        return new While(cond, body)
    }

    parseIf(source) {
        const condCode = this.readUntilBodyOpens(source)
        const cond = this.parseExpression(new Source(condCode), {}, null)
        const body = this.parseBody(source)
        return new If(cond, body)
    }

    parseFunction(source) {
        const args = this.readArguments(source)
        const body = this.parseBody(source)
        return new LangFunction(body, args)
    }

    readArguments(source) {
        consumeSpaces(source)

        if ('(' !== source.currentChar()) {
            throw new LangParseError(Errors.EXPECTED_SYMBOL, '(', source.currentChar())
        }       
        source.move()
        consumeSpaces(source) 

        const args = []
        let first = true
        while((',' === source.currentChar() || ')' !== source.currentChar()) && !source.finished()) {
            if (!first) source.move()
            first = false

            const name = this.readIdentifier(source)
            args.push(name)

            consumeSpaces(source)
        }

        if (')' !== source.currentChar()) {
            throw new LangParseError(Errors.EXPECTED_SYMBOL, ')', source.currentChar())
        }
        source.move()
        return args
    }

    parseBody(source) {
        consumeSpaces(source)

        if ('{' !== source.currentChar()) {
            throw new LangParseError(Errors.EXPECTED_SYMBOL, '{', source.currentChar())
        }

        source.move()
        consumeSpaces(source)

        let body = ''
        let openQuotings = 0
        while((openQuotings || '}' !== source.currentChar()) && !source.finished()) {
            const c = source.currentChar()
            body += c
            source.move()

            if ('{' === c) openQuotings++
            if ('}' === c) openQuotings--
        } 

        if ('}' !== source.currentChar()) {
            throw new LangParseError(Errors.EXPECTED_SYMBOL, '}', source.currentChar())
        }
        source.move()

        if (/^\s*$/.test(body)) {
            return new Block([new LangVoid()])
        }

        const block = this.parseBlock(new Source(body))
        return block
    }
    
    readString(source, quoting) {
        let token = ''
        for (; !source.finished(); source.move()) {
            const c = source.currentChar()

            if (isStringEnding(c, quoting)) {
                source.move()
                return token
            }
            token += c
        }
        throw new LangParseError(Errors.UNEXPECTED_END)
    }

    readIdentifier(source) {
        let token = ''
        for (; !source.finished(); source.move()) {
            const c = source.currentChar()
            if (isSpace(c)) continue
            if (!new RegExp(`^${RE_IDENTIFIER}$`).test(token + c)) break                
            token += c
        }
        if (token) return token
        throw new LangParseError(Errors.EXPECTED_IDENTIFIER)
    }

    readUntilBodyOpens(source) {
        let curlies = 0
        let quotations = 0
        let token = ''
        for (; !source.finished(); source.move()) {
            const c = source.currentChar()
            if (isStringStarting(c)) {
                token += this.readString(source, c)
                continue
            }
            if ('(' === c) quotations++
            else
            if (')' === c) quotations--
            else
            if ('{' === c) {
                if (!curlies && !quotations) break
                curlies++
            } else 
            if ('}' === c) {
                curlies--
                if (curlies < 0) throw new LangParseError(Errors.UNEXPECTED_SYMBOL, c)
            }
            token += c
        }
        if (token) return token
    }

    _stepper() {
        this.steps++
        if (this.steps > this.maxSteps) throw new LangParseError(Errors.PARSER_STEPS_EXCEEDED)
    }
}

function consumeSpaces(source, stopAtNewLine = false) {
    while (!source.finished() && /\s/.test(source.currentChar()) && (!stopAtNewLine || '\n' !== source.currentChar())) source.move()
}

function consumeUntil(source, untilChar) {
    const re = new RegExp(untilChar)
    while (!source.finished() && !re.test(source.currentChar())) source.move()
}

function isNumeric(str) {
    return !isNaN(str) && !isNaN(parseFloat(str))
}

function isKeyword(str) {
    str = str.toLowerCase()
    return Object.values(Keywords).some(k => k.includes(str)) ||  WhileKeywords.includes(str) || IfKeywords.includes(str)
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

function isIdentifier(token) {
    return new RegExp(`^${RE_IDENTIFIER}$`).test(token)
}

function isFunctionDef(remaining) {
    return new RegExp(`^${RE_FUNCTION}`).test(remaining)
}

function isWhileDef(remaining) {
    return WhileKeywords.some(k => new RegExp(`^\\s*${k}\\s(.*)\\s{`).test(remaining)) 
}

function isIfDef(remaining) {
    return IfKeywords.some(k => new RegExp(`^\\s*${k}\\s(.*)\\s{`).test(remaining)) 
}

function isElseDef(remaining) {
    return ElseKeywords.some(k => new RegExp(`^\\s*${k}\\s+{`).test(remaining)) 
}

function isComment(remaining) {
    return new RegExp('^//').test(remaining)
}

module.exports = Parser