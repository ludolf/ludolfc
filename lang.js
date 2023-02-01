const Keywords = {
    TRUE: ['true', 'pravda', 'wahr'],
    FALSE: ['false', 'nepravda', 'unwahr'],
    IF: ['if', 'pokud', 'falls'],
    ELSE: ['else', 'jinak', 'sonst'],
    WHILE: ['while', 'dokud', 'soweit'],
}
const SizeKeywords = ['size', 'velikost', 'größe']
const WhileKeywords = ['while', 'dokud', 'solange']
const IfKeywords = ['if', 'pokud', 'falls']
const ElseKeywords = ['else', 'jinak', 'sonst']

const Errors = {
    INVALID_UNI_OPERATOR: 'INVALID_UNI_OPERATOR',
    INVALID_BI_OPERATOR: 'INVALID_BI_OPERATOR',
    UNEXPECTED_END: 'UNEXPECTED_END',
    UNEXPECTED_SYMBOL: 'UNEXPECTED_SYMBOL',
    EXPECTED_SYMBOL: 'EXPECTED_SYMBOL',
    UNREFERENCED_VARIABLE: 'UNREFERENCED_VARIABLE',
    UNEXPEXTED_KEYWORD: 'UNEXPEXTED_KEYWORD',
    INVALID_IDENTIFIER: 'INVALID_IDENTIFIER',
    UNEVEN_OPERATORS: 'UNEVEN_OPERATORS',
    EXPEXTED_FUNCTION: 'EXPEXTED_FUNCTION',
    EXPEXTED_STATEMENT_END: 'EXPEXTED_STATEMENT_END',
    ATTRIBUTE_NOT_EXISTS: 'ATTRIBUTE_NOT_EXISTS',
    ARRAY_INDEX_NOT_NUMBER: 'ARRAY_INDEX_NOT_NUMBER',
    ARRAY_INDEX_MISSING: 'ARRAY_INDEX_MISSING',
    ARRAY_INDEX_OUT_BOUNDS: 'ARRAY_INDEX_OUT_BOUNDS',
    FUNC_ARGUMENTS_MISHMASH: 'FUNC_ARGUMENTS_MISHMASH',
    ATTRIBUTE_ALREADY_EXISTS: 'ATTRIBUTE_ALREADY_EXISTS',
    EMPTY_EXPRESSION: 'EMPTY_EXPRESSION',
    UNKNOWN_OPERATOR: 'UNKNOWN_OPERATOR',
    OPERATOR_NOT_APPLICABLE: 'OPERATOR_NOT_APPLICABLE',
    ACCESS_OPERATOR_EXPECTED: 'ACCESS_OPERATOR_EXPECTED',
    WRONG_UNI_OPERATOR_SUBJECT: 'WRONG_UNI_OPERATOR_SUBJECT',
    WRONG_BI_OPERATOR_SUBJECTS: 'WRONG_BI_OPERATOR_SUBJECTS',
    UNMATCHING_BI_OPERATOR_SUBJECTS: 'UNMATCHING_BI_OPERATOR_SUBJECTS',
    EXPECTED_ARRAY: 'EXPECTED_ARRAY',
    EXPECTED_OBJECT: 'EXPECTED_OBJECT',
    WRONG_ASSIGNMENT: 'WRONG_ASSIGNMENT',
    WRONG_ASSIGNEE_TYPE: 'WRONG_ASSIGNEE_TYPE',
    READONLY_ATTRIBUTE: 'READONLY_ATTRIBUTE',
    WRONG_CONDITION: 'WRONG_CONDITION',
    WRONG_CONDITION_VALUE: 'WRONG_CONDITION_VALUE',
    EXECUTION_STEPS_EXCEEDED: 'EXECUTION_STEPS_EXCEEDED',
    PARSER_STEPS_EXCEEDED: 'PARSER_STEPS_EXCEEDED',
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
    constructor(id, arg1, arg2) {
        super(`Error ${id} ${arg1 ? `"${arg1}"` : ''} ${arg2 ? `"${arg2}"` : ''}`)
        this.id = id
        this.arg1 = arg1
        this.arg2 = arg2
    }
}

class LangParseError extends LangError {
    constructor(id, arg1, arg2) {
        super(id, arg1, arg2)
    }
}

class LangInterpretError extends LangError {
    constructor(id, arg1, arg2) {
        super(id, arg1, arg2)
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
        const fn = getFn(this.op)
        if (!fn || !fn.call) throw new LangError(Errors.OPERATOR_NOT_APPLICABLE, this.op)
        return fn.call()

        function getFn(op) {
            switch (op) {
                case '!': 
                case '-': return a.neg
                default: throw new LangError(Errors.INVALID_UNI_OPERATOR, this.op)
            }
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
        const fn = getFn(this.op)
        if (!fn || !fn.call) {
            if ('=' === this.op) return new LangBoolean(false)
            throw new LangError(Errors.OPERATOR_NOT_APPLICABLE, this.op)
        }
        return fn.call(b)

        function getFn(op) {
            switch (op) {
                case '*': return a.mult
                case '/': return a.div
                case '%': return a.mod
                case '+': return a.plus
                case '-': return a.minus
                case '<': return a.lt
                case '<=': return a.le
                case '>': return a.gt
                case '>=': return a.ge
                case '=': return a.eq
                case '!=': return a.ne
                case '&': return a.and
                case '|': return a.or
                default: throw new LangError(Errors.INVALID_BI_OPERATOR, this.op)
            }
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
    constructor(obj, type = Types.OBJECT) {
        this.value = obj
        this.type = type
        this.isObject = true
        this.parent = null

        this.eq = new LangNativeFunction(x => new LangBoolean(areObjectsEqual(this, x)))
        this.ne = new LangNativeFunction(x => new LangBoolean(!(this.eq.call(x).value)))
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
        
        this.plus = this.concat
        for (let s of SizeKeywords) this[s] = new LangNumber(this.value.length)
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

        this.plus = this.concat
        for (let s of SizeKeywords) this[s] = new LangNumber(this.value.length)

        this.eq = new LangNativeFunction(x => {
            if (!x || !x.value) return new LangBoolean(false)
            if (this.value.length !== x.value.length) return new LangBoolean(false)
            for (let i = 0; i < this.value.length; i++)
                if (!this.value[i].eq || !this.value[i].eq.isNative || !this.value[i].eq.call(x.value[i]).value) return new LangBoolean(false)
            return new LangBoolean(true)
        })
        this.ne = new LangNativeFunction(x => new LangBoolean(!(this.eq.call(x).value)))
    }
    element(indexes, newValue) {
        return indexes.reduce((a,c,i) => {
            const index = Math.ceil(c.value)
            if (index >= a.value.length) throw new LangError(Errors.ARRAY_INDEX_OUT_BOUNDS)
            const v = a.value[index]
            // set the value for the last element
            if (newValue && i === indexes.length - 1)
                a.value[index] = newValue            
            return v
        }, this)
    }
    attribute(name, newValue) {
        if (SizeKeywords.includes(name.toLowerCase())) {
            if (newValue) throw new LangError(Errors.READONLY_ATTRIBUTE)
            return new LangNumber(this.value.length)
        }
        return super.attribute(name, newValue)
    }
    hasAttribute(name) {
        return SizeKeywords.includes(name.toLowerCase()) || super.attribute(name, newValue)
    }
}

class LangFunction {
    constructor(body, args) {
        this.type = Types.FUNCTION
        this.body = body
        this.args = args
        this.isFunction = true
        this.eq = new LangNativeFunction(x => new LangBoolean(false))
        this.ne = new LangNativeFunction(x => new LangBoolean(true))
    }
}

class LangNativeFunction {
    constructor(func) {
        this.type = Types.FUNCTION
        this.func = func
        this.isNative = true
    }
    call(...params) {
        return this.func(...params)
    }
}

class LangVoid extends LangValueObject {
    constructor() {
        super(null, Types.VOID)

        this.eq = new LangNativeFunction(x => new LangBoolean(false))
        this.ne = new LangNativeFunction(x => new LangBoolean(false))
    }
}

function areObjectsEqual(a, b) {
    const aKeys = Object.keys(a.value)
    const bKeys = Object.keys(b.value)
    if (aKeys.length !== bKeys.length) return false
    for (k of aKeys) {
        if (!a.value[k].eq.call(b.value[k])) return false
    }
    return true
}

module.exports = {
    Keywords,
    Errors,
    Types,
    SizeKeywords,
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
    LangParseError,
    LangInterpretError,
    LangObject,
    LangNumber,
    LangString,
    LangBoolean,
    LangArray,
    LangFunction,
    LangNativeFunction,
    LangVoid,
}