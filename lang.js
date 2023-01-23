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
    ATTRIBUTE_ALREADY_EXISTS: 'ATTRIBUTE_ALREADY_EXISTS',
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

class LangError extends Error {
    constructor(id, row, col, arg1, arg2) {
        super(`Error ${id} at ${row}:${col}`)
        this.id = id
        this.arg1 = arg1
        this.arg2 = arg2
    }
}

class Block {
    constructor(statements) {
        this.statements = statements
    }
}

class Statement {
    constructor() {
        this.isExpression = false
        this.isAssignment = false
        this.isWhile = false
        this.isIf = false
    }
}

class Assignment extends Statement {
    constructor(left, right) {
        super()
        this.isAssignment = true
        this.left = left
        this.right = right
    }
}

class While extends Statement {
    constructor(condition, body) {
        super()
        this.isWhile = true
        this.condition = condition
        this.body = body
    }
}

class If extends Statement {
    constructor(condition, body, bodyElse) {
        super()
        this.isIf = true
        this.condition = condition
        this.body = body
        this.elseBody = bodyElse
    }
}

class Expression extends Statement {
    constructor(parts) {
        super()
        this.isExpression = true
        this.parts = parts
    }
}

class Variable {
    constructor(name) {
        this.isVariable = true
        this.name = name
    }
}

class Operator {
    constructor(op, precedence = -1) {
        this.op = op
        this.isOperator = true
        this.precedence = precedence
    }
}

class UniOperator extends Operator {
    constructor(op) {
        super(op)
        this.isUni = true
        this.precedence = this.getPrecedence()
    }

    apply(a) {
        switch (this.op) {
            case '!': 
            case '-': return a.neg.call()
            default: throw new Error('Invalid uni operator ' + this.op)
        }
    }

    getPrecedence() { // based on https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
        switch (this.op) {
            case '!':
            case '-': return 14
            default: -1
        }
    }
}

class BiOperator extends Operator {
    constructor(op) {
        super(op)
        this.isBi = true
        this.precedence = this.getPrecedence()
    }

    apply(a, b) {
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

    getPrecedence() { // based on https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
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

class ArrayAccess extends Operator {
    constructor(indexes) {
        super('[]', 17)
        this.isAccess = true
        this.isArrayAccess = true
        this.indexes = indexes
    }

    apply(a, indexes, newValue) { // indexes are resolved, but this.indexes are AST (expressions)
        return a.element(indexes, newValue)
    }
}

class ObjectAccess extends Operator {
    constructor(attrName) {
        super('.', 17)
        this.isAccess = true
        this.isObjectAccess = true
        this.attrName = attrName
    }

    apply(o, newValue) {
        return o.attribute(this.attrName, newValue)
    }
}

class FunctionCall extends Operator {
    constructor(params) {
        super('()', 17)
        this.isAccess = true
        this.isCall = true
        this.params = params
    }
}

class VarReference {
    constructor(varName) {
        this.isReference = true
        this.varName = varName
    }
}

class LangObject {
    constructor(value, type = Types.OBJECT) {
        this.value = value
        this.type = type
        this.isObject = true
        this.parent = null
    }
    attribute(name, newValue) {
        const value = this[name] ? this[name] : this.value[name] // explicit attrs have priority over native ones
        if (newValue && this.value[name]) this.value[name] = newValue
        if (value) return value
        if (this.parent) return this.parent.attribute(name)        
    }
    hasAttribute(name) {
        const hasValue = this[name] || this.value[name]
        return hasValue || (this.parent && this.parent.hasAttribute(name))
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

        this.mult = this.and
        this.plus = this.or
    }
}

class LangArray extends LangValueObject {
    constructor(value) {
        super(value, Types.ARRAY)

        this.concat = new LangNativeFunction(x => new LangArray(this.value.concat(x.value)))
        this.length = new LangNativeFunction(() => new LangNumber(this.value.length))
    }
    element(indexes, newValue) {
        return indexes.reduce((a,c,i) => {
            const v = a.value[Math.ceil(c.value)]
            // set the value for the last element
            if (newValue && i === indexes.length - 1)
                a.value[Math.ceil(c.value)] = newValue            
            return v
        }, this)
    }
}

class LangFunction extends LangObject {
    constructor(body, args) {
        super(body, Types.FUNCTION)
        this.args = args
    }
    call(interpret, ...params) {
        if ((!params && this.args) || params.length !== this.args.length) {
            throw new LangError(Errors.FUNC_ARGUMENTS_MISHMASH, interpret.source.row, interpret.source.col)
        }
        // cache scoped variables
        const cache = {}
        let i = 0
        for (let arg of this.args) {
            if (interpret.variables.has(arg)) {
                cache[arg] = interpret.variables.get(arg)
            }
            interpret.variables.set(arg, params[i++])
        }
        if (this.parent) {
            // cache "this" object into variable $
            if (interpret.variables.has('$')) {
                cache['$'] = interpret.variables.get('$')
            }
            interpret.variables.set('$', this.parent)
        }
        
        try {
            return interpret._execProgram(new Source(this.value))

        } finally {  // clean up variables
            if (cache['$'])
                interpret.variables.set('$', cache['$'])
            else 
                interpret.variables.delete('$')

            for (let arg of this.args) {
                if (cache[arg]) 
                    interpret.variables.set(arg, cache[arg])
                else
                    interpret.variables.delete(arg)
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

module.exports = {
    Keywords,
    Errors,
    Types,
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
    LangError,
    LangObject,
    LangNumber,
    LangString,
    LangBoolean,
    LangArray,
    LangFunction,
    LangNativeFunction,
    LangVoid,
}