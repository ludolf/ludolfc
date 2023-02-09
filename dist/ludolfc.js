(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory();
	else if(typeof define === 'function' && define.amd)
		define([], factory);
	else if(typeof exports === 'object')
		exports["ludolfc"] = factory();
	else
		root["ludolfc"] = factory();
})(this, () => {
return /******/ (() => { // webpackBootstrap
/******/ 	var __webpack_modules__ = ({

/***/ "./src/interpret.js":
/*!**************************!*\
  !*** ./src/interpret.js ***!
  \**************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const { 
    Types,
    Errors,
    Interruptions,
    InterpretError: LangInterpretError,
    Interrupt: LangInterrupt,
    Void: LangVoid } = __webpack_require__(/*! ./lang */ "./src/lang.js")

class Interpret {
    constructor(imports = {}, controls, maxSteps = 100000) {
        this.variables = new VariablesScope(imports)
        this.stepper = new ExecutionStepper(maxSteps, controls && controls.isInterrupted) // to prevent infinite loops
    }

    async execute(ast) {
        this.variables.clear()
        this.stepper.reset()
        return await this.executeBlock(ast, false)
    }

    async executeBlock(block, newScope = true) {        
        if (newScope) this.variables.pushScope()
        let result
        for (let stm of block.statements) {
            result = await this.executeStatement(stm)
        }
        if (newScope) this.variables.popScope()
        return result ? result : new LangVoid()
    }

    async executeStatement(stm) {
        this.stepper.step(stm.source)
        return stm.isExpression ? await this.executeExpression(stm) :
               stm.isAssignment ? await this.executeAssignment(stm) :
               stm.isWhile ? await this.executeWhile(stm) :
               stm.isIf ? await this.executeIf(stm) : 
               stm
    }

    async executeExpression(expression, assignNewValue = null) {
        this.stepper.step(expression.source)
        if (!expression.parts) throw new LangInterpretError(Errors.EMPTY_EXPRESSION, expression.source)
        let parts = [...expression.parts]
        return await this.executeExpressionParts(parts, assignNewValue)
    }

    async executeExpressionParts(parts, assignNewValue = null) {
        // logical operators short circuit
        let index = findFirstOp('&')
        if (index) {
            const left = await this.executeExpressionParts(parts.slice(0, index), assignNewValue)
            if (left.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS, left.source)
            if (!left.value) return left
            const right = await this.executeExpressionParts(parts.slice(index + 1), assignNewValue)
            if (right.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS, right.source)
            return right
        }
        index = findFirstOp('|')
        if (index) {
            const left = await this.executeExpressionParts(parts.slice(0, index), assignNewValue)
            if (left.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS, left.source)
            if (left.value) return left
            const right = await this.executeExpressionParts(parts.slice(index + 1), assignNewValue)
            if (right.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS, right.source)
            return right
        }

        // left to right by precendence
        let assignApplied = false
        while ((index = findNextOp()) > -1) {
            const op = parts[index]

            if (assignNewValue && !op.isObjectAccess && !op.isArrayAccess) throw new LangInterpretError(Errors.ACCESS_OPERATOR_EXPECTED, op.source)

            try {
                if (op.isUni) {
                    const a = await this.executeExpressionPart(parts[index + 1])
                    if (!a.type) throw new LangInterpretError(Errors.WRONG_UNI_OPERATOR_SUBJECT, op.source)
                    parts[index] = op.apply(a)
                    parts = removeElementAt(parts, index + 1)
                } else 
                if (op.isBi) {
                    const a = await this.executeExpressionPart(parts[index - 1])
                    const b = await this.executeExpressionPart(parts[index + 1])
                    if (!a.type || !b.type) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS, op.source)
                    if (a.type !== b.type) throw new LangInterpretError(Errors.UNMATCHING_BI_OPERATOR_SUBJECTS, op.source)
                    parts[index] = op.apply(a, b)
                    parts = removeElementAt(parts, index - 1, index + 1)
                } else 
                if (op.isArrayAccess) {
                    const a = await this.executeExpressionPart(parts[index - 1])
                    if (Types.ARRAY !== a.type) throw new LangInterpretError(Errors.EXPECTED_ARRAY, op.source)
                    if (assignNewValue && a.protected()) throw new LangInterpretError(Errors.PROTECTED_FROM_MODIFICATION, op.source)
                    const indexes = await Promise.all(op.indexes.map(i => this.executeExpressionPart(i)))
                    parts[index] = op.apply(a, indexes, (assignNewValue && isLastOperator()) ? assignNewValue : null)
                    if (!parts[index]) throw new LangInterpretError(Errors.ATTRIBUTE_NOT_FOUND, op.source)
                    parts = removeElementAt(parts, index - 1)
                    assignApplied = true
                } else 
                if (op.isObjectAccess) {
                    const o = await this.executeExpressionPart(parts[index - 1])
                    if (!o.isObject) throw new LangInterpretError(Errors.EXPECTED_OBJECT, op.source)
                    if (assignNewValue && o.protected()) throw new LangInterpretError(Errors.PROTECTED_FROM_MODIFICATION, op.source)
                    parts[index] = op.apply(o, (assignNewValue && isLastOperator()) ? assignNewValue : null)
                    if (!parts[index]) throw new LangInterpretError(Errors.ATTRIBUTE_NOT_FOUND, op.source)
                    parts = removeElementAt(parts, index - 1)
                    assignApplied = true
                } else 
                if (op.isCall) {
                    const f = await this.executeExpressionPart(parts[index - 1])
                    const params = await Promise.all(op.params.map(p => this.executeExpressionPart(p)))
                    parts[index] = await this.executeFunctionCall(f, params)
                    parts = removeElementAt(parts, index - 1)
                }
                else throw new LangInterpretError(Errors.UNKNOWN_OPERATOR, op.source)

            } catch (e) {   // LangError could be raised from applying the operator in lang.js
                if (e.isLangError) {
                    if (!e.isInterpretError) throw new LangInterpretError(e.id, op.source, e.arg1, e.arg2)
                } else {
                    throw new LangInterpretError(Errors.UNKNOWN_ERROR, op.source, e)
                }
                throw e
            }
        }

        if (assignNewValue && !assignApplied) throw new LangInterpretError(Errors.ACCESS_OPERATOR_EXPECTED, parts[0].source)

        return await this.executeExpressionPart(parts[0]) // parts are reduced to a single result

        function findNextOp() { // returns an index of the next part
            let index = -1
            let maxPrecedence = Number.MIN_SAFE_INTEGER
            for (let i = 0; i < parts.length; i++) {
                const op = parts[i]
                if (!op.isOperator) continue
                if (maxPrecedence < op.precedence || (op.isUni && maxPrecedence === op.precedence)) {
                    index = i
                    maxPrecedence = op.precedence
                }
            }
            return index
        }

        function removeElementAt(arr, ...indexes) {
            return arr.filter((_, i) => !indexes.includes(i))
        }

        function isLastOperator() {
            return parts.length === 2
        }

        function findFirstOp(op) {
            for (let i = 0; i < parts.length; i++)
                if (parts[i].isBi && parts[i].op === op) return i
        }
    }

    async executeExpressionPart(expressionPart) {
        this.stepper.step(expressionPart.source)
        
        if (expressionPart.isReference) {
            if (!this.variables.hasVariable(expressionPart.varName)) throw new LangInterpretError(Errors.UNREFERENCED_VARIABLE, expressionPart.source - expressionPart.varName.length, expressionPart.varName)
            return this.variables.getVariable(expressionPart.varName)
        }
        if (expressionPart.isExpression) {
            return await this.executeExpression(expressionPart)
        }
        if (Types.ARRAY === expressionPart.type) {
            const arr = expressionPart.value
            for (let i = 0; i < arr.length; i++) {
                arr[i] = await this.executeExpressionPart(arr[i])
            }
        } else
        if (Types.OBJECT === expressionPart.type) {
            const obj = expressionPart.value
            for (let k of Object.keys(obj)) {
                obj[k] = await this.executeExpressionPart(obj[k])
                if (obj[k].isObject || obj[k].isFunction) obj[k].parent = expressionPart
            }
        }
        return expressionPart
    }

    async executeFunctionCall(f, params) {
        if (f.isNative) {
            const result = await f.call(...params)
            return !result ? new LangVoid() : result
        }

        if ((!params && f.args) || params.length !== f.args.length) throw new LangInterpretError(Errors.FUNC_ARGUMENTS_MISHMASH, f.source)
        // scoped variables
        let i = 0
        this.variables.pushScope()
        for (let arg of f.args) {
            this.variables.setVariable(arg, params[i++], true)
        }
        if (f.parent) {
            // cache "this" object into variable $
            this.variables.setVariable('$', f.parent, true)
        }
        
        try {
            const result = await this.executeBlock(f.body, false)
            return result

        } finally {  // clean up variables
            this.variables.popScope()
        }
    }

    async executeAssignment(assignment) {
        if (!assignment.left || !assignment.right) throw new LangInterpretError(Errors.WRONG_ASSIGNMENT, assignment.source)
        const value = await this.executeExpressionPart(assignment.right)        
        // variable assignment
        if (assignment.left.isVariable) {
            const variable = this.variables.getVariable(assignment.left.name)
            if (variable.protected && variable.protected()) throw new LangInterpretError(Errors.PROTECTED_FROM_MODIFICATION, assignment.left.source)
            this.variables.setVariable(assignment.left.name, value)
        } else
        // object attribute or array element assignment
        if (assignment.left.isExpression) {
            await this.executeExpression(assignment.left, value)
        }
        else throw new LangInterpretError(Errors.WRONG_ASSIGNEE_TYPE)
    }

    async executeWhile(whileStm) {
        if (!whileStm.condition || !whileStm.condition.isExpression) throw new LangInterpretError(Errors.WRONG_CONDITION, whileStm.source)
        while (true) {
            const cond = await this.executeExpressionPart(whileStm.condition)
            if (cond.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_CONDITION_VALUE, cond.source)
            if (cond.value) await this.executeBlock(whileStm.body)
            else break
        } 
    }

    async executeIf(ifStm) {
        if (!ifStm.condition || !ifStm.condition.isExpression) throw new LangInterpretError(Errors.WRONG_CONDITION, ifStm.source)
        const cond = await this.executeExpressionPart(ifStm.condition)
        if (cond.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_CONDITION_VALUE, cond.source)
        if (cond.value) await this.executeBlock(ifStm.body)
        else if (ifStm.elseBody) await this.executeBlock(ifStm.elseBody)
    }
}

class VariablesScope {
    constructor(imports = {}) {
        this.variables = [new Map()]
        this.imports = imports
    }

    clear() {
        this.variables = [new Map()]
        if (this.imports) Object.entries(this.imports).forEach(([k,v]) => this.variables[0].set(k, v))
    }

    hasVariable(name) {
        for (let i = this.variables.length - 1; i >= 0; i--) {
            if (this.variables[i].has(name)) return true
            if (this.variables[i].has('$')) {
                const self = this.variables[i].get('$')
                if (self.hasAttribute(name)) return true
            }
        }
        return false
    }

    getVariable(name) {
        for (let i = this.variables.length - 1; i >= 0; i--) {
            if (this.variables[i].has(name)) return this.variables[i].get(name)
            if (this.variables[i].has('$')) {
                const self = this.variables[i].get('$')
                if (self.hasAttribute(name)) return self.attribute(name)
            }
        }
        return false
    }

    setVariable(name, value, scoped = false) {
        if (scoped) {
            this.variables[this.variables.length - 1].set(name, value)
            return
        }
        let found = false
        for (let i = this.variables.length - 1; i >= 0; i--) {
            if (this.variables[i].has(name)) {
                this.variables[i].set(name, value)
                found = true
                break
            }
        }
        if (!found) {
            if (!this.variables.length) this.clear()
            this.variables[this.variables.length - 1].set(name, value)
        }
    }

    pushScope() {
        this.variables.push(new Map())
    }

    popScope() {
        this.variables.pop()
    }
}

class ExecutionStepper {
    constructor(maxSteps, isInterruptedFn) {
        this.maxSteps = maxSteps
        this.isInterruptedFn = isInterruptedFn
        this.steps = 0        
    }

    step(source) {
        if (this.isInterruptedFn && this.isInterruptedFn()) throw new LangInterrupt(Interruptions.USER_SUSSPEND)
        this.steps++
        if (this.steps > this.maxSteps) throw new LangInterpretError(Errors.EXECUTION_STEPS_EXCEEDED, source)
    }

    reset(maxSteps = null) {
        this.steps = 0
        if (maxSteps) this.maxSteps = maxSteps
    }
}

module.exports = Interpret

/***/ }),

/***/ "./src/lang.js":
/*!*********************!*\
  !*** ./src/lang.js ***!
  \*********************/
/***/ ((module) => {

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
    ARRAY_INDEX_NOT_NUMBER: 'ARRAY_INDEX_NOT_NUMBER',
    ARRAY_INDEX_MISSING: 'ARRAY_INDEX_MISSING',
    ARRAY_INDEX_OUT_BOUNDS: 'ARRAY_INDEX_OUT_BOUNDS',
    FUNC_ARGUMENTS_MISHMASH: 'FUNC_ARGUMENTS_MISHMASH',
    ATTRIBUTE_ALREADY_EXISTS: 'ATTRIBUTE_ALREADY_EXISTS',
    ATTRIBUTE_NOT_FOUND: 'ATTRIBUTE_NOT_FOUND',
    ELEMENT_NOT_FOUND: 'ELEMENT_NOT_FOUND',
    EMPTY_EXPRESSION: 'EMPTY_EXPRESSION',
    UNKNOWN_OPERATOR: 'UNKNOWN_OPERATOR',
    OPERATOR_NOT_APPLICABLE: 'OPERATOR_NOT_APPLICABLE',
    ACCESS_OPERATOR_EXPECTED: 'ACCESS_OPERATOR_EXPECTED',
    WRONG_UNI_OPERATOR_SUBJECT: 'WRONG_UNI_OPERATOR_SUBJECT',
    WRONG_BI_OPERATOR_SUBJECTS: 'WRONG_BI_OPERATOR_SUBJECTS',
    UNMATCHING_BI_OPERATOR_SUBJECTS: 'UNMATCHING_BI_OPERATOR_SUBJECTS',
    EXPECTED_ARRAY: 'EXPECTED_ARRAY',
    EXPECTED_OBJECT: 'EXPECTED_OBJECT',
    EXPECTED_IDENTIFIER: 'EXPECTED_IDENTIFIER',
    WRONG_ASSIGNMENT: 'WRONG_ASSIGNMENT',
    WRONG_ASSIGNEE_TYPE: 'WRONG_ASSIGNEE_TYPE',
    READONLY_ATTRIBUTE: 'READONLY_ATTRIBUTE',
    WRONG_CONDITION: 'WRONG_CONDITION',
    WRONG_CONDITION_VALUE: 'WRONG_CONDITION_VALUE',
    EXECUTION_STEPS_EXCEEDED: 'EXECUTION_STEPS_EXCEEDED',
    PARSER_STEPS_EXCEEDED: 'PARSER_STEPS_EXCEEDED',
    PROTECTED_FROM_MODIFICATION: 'PROTECTED_FROM_MODIFICATION',
    DIVISION_BY_ZERO: 'DIVISION_BY_ZERO',
    UNKNOWN_ERROR: 'UNKNOWN_ERROR',
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

const Interruptions = {
    USER_SUSSPEND: 'USER_SUSSPEND',
}

class LangInterrupt {
    constructor(id) {
        this.id = id
        this.isLangInterruption = true
    }
}

class LangError extends Error {
    constructor(id, pos, arg1, arg2) {
        super(id)
        this.message = `${id} ${arg1 ? `"${arg1}"` : ''} ${arg2 ? `"${arg2}"` : ''}`
        this.id = id
        this.arg1 = arg1
        this.arg2 = arg2
        this.position = pos
        this.isLangError = true
    }
}

class LangParseError extends LangError {
    constructor(id, pos, arg1, arg2) {
        super(id, pos, arg1, arg2)
        this.isParseError = true
    }
}

class LangInterpretError extends LangError {
    constructor(id, pos, arg1, arg2) {
        super(id, pos, arg1, arg2)
        this.isInterpretError = true
    }
}

class Block {
    constructor(statements, source) {
        this.statements = statements
        this.source = source
    }
}

class Statement {
    constructor(source) {
        this.isExpression = false
        this.isAssignment = false
        this.isWhile = false
        this.isIf = false
        this.source = source
    }
}

class Assignment extends Statement {
    constructor(left, right, source) {
        super(source)
        this.isAssignment = true
        this.left = left
        this.right = right
    }
}

class While extends Statement {
    constructor(condition, body, source) {
        super(source)
        this.isWhile = true
        this.condition = condition
        this.body = body
    }
}

class If extends Statement {
    constructor(condition, body, bodyElse, source) {
        super(source)
        this.isIf = true
        this.condition = condition
        this.body = body
        this.elseBody = bodyElse
    }
}

class Expression extends Statement {
    constructor(parts, source) {
        super(source)
        this.isExpression = true
        this.parts = parts
    }
}

class Variable {
    constructor(name, source) {
        this.isVariable = true
        this.name = name
        this.source = source
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
    constructor(op, source) {
        super(op)
        this.isUni = true
        this.precedence = this.getPrecedence()
        this.source = source
    }
    apply(a) {
        const fn = getFn(this.op)
        if (!fn || !fn.call) throw new LangError(Errors.OPERATOR_NOT_APPLICABLE, null, this.op)
        return fn.call()

        function getFn(op) {
            switch (op) {
                case '!': 
                case '-': return a.neg
                default: throw new LangError(Errors.INVALID_UNI_OPERATOR, null, this.op)
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
    constructor(op, source) {
        super(op)
        this.isBi = true
        this.precedence = this.getPrecedence()
        this.source = source
    }
    apply(a, b) {
        const fn = getFn(this.op)
        if (!fn || !fn.call) {
            if ('=' === this.op) return new LangBoolean(false)
            throw new LangError(Errors.OPERATOR_NOT_APPLICABLE, null, this.op)
        }
        if ('/' === this.op && Types.NUMBER === b.type && b.value === 0) {
            throw new LangError(Errors.DIVISION_BY_ZERO, null, this.op)
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
                default: throw new LangError(Errors.INVALID_BI_OPERATOR, null, this.op)
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
    constructor(indexes, source) {
        super('[]', 17)
        this.isAccess = true
        this.isArrayAccess = true
        this.indexes = indexes
        this.source = source
    }
    apply(a, indexes, newValue) { // indexes are resolved, but this.indexes are AST (expressions)
        return a.element(indexes, newValue)
    }
}

class ObjectAccess extends Operator {
    constructor(attrName, source) {
        super('.', 17)
        this.isAccess = true
        this.isObjectAccess = true
        this.attrName = attrName
        this.source = source
    }
    apply(o, newValue) {
        return o.attribute(this.attrName, newValue)
    }
}

class FunctionCall extends Operator {
    constructor(params, source) {
        super('()', 17)
        this.isAccess = true
        this.isCall = true
        this.params = params
        this.source = source
    }
}

class VarReference {
    constructor(varName, source) {
        this.isReference = true
        this.varName = varName
        this.source = source
    }
}

class LangObject {
    constructor(obj, source, type = Types.OBJECT) {
        this.value = obj
        this.type = type
        this.isObject = true
        this.parent = null
        this.source = source

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
    protected() {
        return this.isProtected || (this.parent && this.parent.protected())
    }
}

class LangValueObject extends LangObject {
    constructor(value, source, type) {
        super(value, source, type)

        this.eq = new LangNativeFunction(x => new LangBoolean(this.value === x.value))
        this.ne = new LangNativeFunction(x => new LangBoolean(this.value !== x.value))
    }
}

class LangNumber extends LangValueObject {
    constructor(value, source) {
        super(value, source, Types.NUMBER)
        
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
    constructor(value, source) {
        super(value, source, Types.STRING)

        this.concat = new LangNativeFunction(x => new LangString(this.value + x.value))
        this.length = new LangNativeFunction(() => new LangNumber(this.value.length))
        
        this.plus = this.concat
        for (let s of SizeKeywords) this[s] = new LangNumber(this.value.length)
    }
}

class LangBoolean extends LangValueObject {
    constructor(value, source) {
        super(value, source, Types.BOOLEAN)

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
    constructor(value, source) {
        super(value, source, Types.ARRAY)

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
            if (index < 0 || index >= a.value.length) throw new LangError(Errors.ARRAY_INDEX_OUT_BOUNDS)
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

class LangVoid extends LangValueObject {
    constructor(source) {
        super(null, source, Types.VOID)

        this.eq = new LangNativeFunction(x => new LangBoolean(false))
        this.ne = new LangNativeFunction(x => new LangBoolean(false))
    }
}

class LangFunction {
    constructor(body, args, source) {
        this.type = Types.FUNCTION
        this.body = body
        this.args = args
        this.isFunction = true
        this.source = source

        this.eq = new LangNativeFunction(x => new LangBoolean(false))
        this.ne = new LangNativeFunction(x => new LangBoolean(true))
    }
}

class LangNativeFunction {
    constructor(func, source) {
        this.type = Types.FUNCTION
        this.func = func
        this.isNative = true
        this.source = source
    }
    call(...params) {
        return this.func(...params)
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
    Interruptions,
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
    ParseError: LangParseError,
    InterpretError: LangInterpretError,
    Interrupt: LangInterrupt,
    Object: LangObject,
    Number: LangNumber,
    String: LangString,
    Boolean: LangBoolean,
    Array: LangArray,
    Function: LangFunction,
    NativeFunction: LangNativeFunction,
    Void: LangVoid,
}

/***/ }),

/***/ "./src/ludolfc.js":
/*!************************!*\
  !*** ./src/ludolfc.js ***!
  \************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const lang = __webpack_require__(/*! ./lang */ "./src/lang.js")
const Parser = __webpack_require__(/*! ./parser */ "./src/parser.js")
const Interpret = __webpack_require__(/*! ./interpret */ "./src/interpret.js")

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
                const {line, col} = lineAndCol(code, e.position)
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
        if ('\n' === code[i]) {
            line++
            col = 1
        }
    }
    col = Math.max(1, col - 1)
    return {line, col}
}

module.exports = { LudolfC, lang }


/***/ }),

/***/ "./src/parser.js":
/*!***********************!*\
  !*** ./src/parser.js ***!
  \***********************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

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
    Void: LangVoid } = __webpack_require__(/*! ./lang */ "./src/lang.js")

const UniOperators = ['!', '-']
const BiOperators = ['*', '/', '%', '+', '-', '<', '<=', '>', '>=', '=', '!=', '&', '|']

const RE_NATIONAL_CHARS = `ěščřžťďýáíéúůüöäñĚŠČŘŽŤĎÝÁÍÉÚŮÜÖÄÑß`
const RE_IDENTIFIER = `[a-zA-Z_${RE_NATIONAL_CHARS}][a-zA-Z0-9_${RE_NATIONAL_CHARS}]*`
const RE_FUNCTION = `\\((\\s*(${RE_IDENTIFIER})\\s*(,\\s*(${RE_IDENTIFIER}))*)?\\s*\\)\\s*\{(.|\\s)*\\}`

class Source {
    constructor(code, startingAt = 0) {
        this.code = code + '\n'
        this.pos = 0
        this.startingAt = startingAt
    }

    move(step = 1) {
        this.pos += step
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

    absPos() {
        return this.pos + this.startingAt - /* last move */ 1 
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
        return new Block(statements, source.absPos())
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
                throw new LangParseError(Errors.EXPECTED_SYMBOL, source.absPos(), expecting, c)
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
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), token)
                }
                consumeSpaces(source)
                consumeUntil(source, '\\s')
                const def = this.parseWhile(source)
                consumeSpaces(source, true)
                if (!isStatementSeparator(source.currentChar())) throw new LangParseError(Errors.EXPEXTED_STATEMENT_END, source.absPos())
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
                if (!isStatementSeparator(source.currentChar())) throw new LangParseError(Errors.EXPEXTED_STATEMENT_END, source.absPos())
                if (!def.elseBody) {
                    consumeSpaces(source)
                    if (isElseDef(source.remaining())) {
                        consumeSpaces(source)
                        consumeUntil(source, '\\s')
                        def.elseBody = this.parseBody(source)
                        consumeSpaces(source, true)
                        if (!isStatementSeparator(source.currentChar())) throw new LangParseError(Errors.EXPEXTED_STATEMENT_END, source.absPos())
                    }
                }
                return def
            }

            if (':' === c && !openDefinitions.objects) {    // assignment starting
                if (!(token.trim().length)) throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), c)
                if (isKeyword(token.trim())) throw new LangParseError(Errors.UNEXPEXTED_KEYWORD, source.absPos(), c)
                expecting = '='
            } else
            if (inAssignment) {  // variable assignment                
                const value = this.parseExpression(source, openDefinitions)
                token = token.trim()
                if (isIdentifier(token)) {
                    const variable = new Variable(token, source.absPos())
                    const assignment = new Assignment(variable, value, source.absPos())
                    return assignment
                } else {
                    const exp = this.parseExpression(new Source(token, source.absPos() - token.length), {})
                    if (!exp || exp.parts.some(p => p.isOperator && !p.isAccess))
                        throw new LangParseError(Errors.INVALID_IDENTIFIER, source.absPos(), token)
                    const assignment = new Assignment(exp, value, source.absPos())
                    return assignment
                }
            }
            else {
                token += c
            }
        }

        // statement is an expression
        if (token.length) {
            const exp = this.parseExpression(new Source(token, source.absPos() - token.length), {})
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
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), c)
                }
                // return the list of tokens and operators
                if (parts.length) {
                    if (parts[parts.length - 1].isOperator && !parts[parts.length - 1].isAccess) {
                        throw new LangParseError(Errors.UNEVEN_OPERATORS, source.absPos())
                    }
                    return new Expression(parts, source.absPos())
                }
                source.move()
                continue
            }

            // function defition
            if (isFunctionDef(source.remaining())) {
                if (parts.length && !parts[parts.length - 1].isOperator) {
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), c)
                }
                const fn = this.parseFunction(source)
                parts.push(fn)
                continue
            }

            // object definition
            if ('{' === c) {
                if (!leftOperatorExpected()) {
                    throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), c)
                }
                source.move()
                const attributes = this.readAttributes(source, ')')

                consumeSpaces(source)
                if ('}' === source.currentChar()) {
                    const obj = new LangObject(attributes, source.absPos())
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
                throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), source.currentChar(), '}')
            }

            // grouping or a function call
            if ('(' === c) {
                source.move()
                if (rightOperatorExpected()) {    // a function call
                    const params = this.readList(source, ')')
                    consumeSpaces(source)
    
                    if (')' === source.currentChar()) {
                        var call = new FunctionCall(params, source.absPos())
                        parts.push(call)
                        source.move()
                    } else {
                        throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), source.currentChar(), ')')
                    }
                } else {    // grouping
                    const exp = this.parseExpression(source, openDefinitions, true)
                    parts.push(exp)
                    consumeSpaces(source)
                    if (')' !== source.currentChar()) throw new LangParseError(Errors.EXPECTED_SYMBOL, source.absPos(), ')')
                    source.move()
                }
                continue
            }

            // object attribute access
            if ('.' === c && rightOperatorExpected()) {
                source.move()
                const attrName = this.readIdentifier(source)               
                parts.push(new ObjectAccess(attrName, source.absPos()))
                continue
            }

            // array
            if ('[' === c) {
                source.move()
                if (rightOperatorExpected()) { // array access
                    const indexes = this.readList(source, ']')
                    consumeSpaces(source)

                    if (!indexes.length) throw new LangParseError(Errors.ARRAY_INDEX_MISSING, source.absPos())

                    if (']' === source.currentChar()) {
                        if (indexes.some(i => !i.isExpression || !i.parts.length)) throw new LangParseError(Errors.ARRAY_INDEX_NOT_NUMBER, source.absPos())
                        source.move()
                        parts.push(new ArrayAccess(indexes, source.absPos()))
                    } else {
                        throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), source.currentChar(), ']')
                    }
                } else {    // array definition
                    const elements = this.readList(source, ']')
                    consumeSpaces(source)
    
                    if (']' === source.currentChar()) {
                        parts.push(new LangArray(elements, source.absPos()))
                        source.move()
                        openDefinitions.arrays--
                    } else {
                        throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), source.currentChar(), ']')
                    }
                }
                continue
            }

            // operators
            if (leftOperatorExpected()) {
                if (UniOperators.includes(c)) {
                    parts.push(new UniOperator(c, source.absPos()))
                    source.move()
                    if (isSpace(source.currentChar())) throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), source.currentChar())
                    continue
                }
            } else 
            if (rightOperatorExpected()) {
                const next2 = source.remaining(2)
                if (BiOperators.includes(next2)) {
                    parts.push(new BiOperator(next2, source.absPos()))
                    source.move(next2.length)
                    continue
                }
                if (BiOperators.includes(c)) {
                    parts.push(new BiOperator(c, source.absPos()))
                    source.move()
                    continue
                }
            }

            if (parts.length && (!parts[parts.length - 1].isOperator || parts[parts.length - 1].isAccess)) 
                throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), source.currentChar())

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
                    return new LangBoolean(true, source.absPos())
                }
                if (Keywords.FALSE.includes(token.toLowerCase())) {
                    return new LangBoolean(false, source.absPos())
                }
                if (isNumeric(token)) {
                    return new LangNumber(token.includes('.') ? parseFloat(token) : parseInt(token), source.absPos())
                } 
                if (isIdentifier(token) || '$' === token) {
                    return new VarReference(token, source.absPos())
                }
                throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), token)
            }

            if (isStringStarting(c)) {
                source.move()
                return new LangString(this.readString(source, c), source.absPos())
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
                    throw new LangParseError(Errors.ATTRIBUTE_ALREADY_EXISTS, source.absPos(), name)
                }
                consumeSpaces(source)

                if (':' !== source.currentChar()) {
                    throw new LangParseError(Errors.EXPECTED_SYMBOL, source.absPos(), ':', source.currentChar())
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
        const cond = this.parseExpression(new Source(condCode, source.absPos() - condCode.length), {}, null)
        const body = this.parseBody(source)
        return new While(cond, body, source.absPos())
    }

    parseIf(source) {
        const condCode = this.readUntilBodyOpens(source)
        const cond = this.parseExpression(new Source(condCode, source.absPos() - condCode.length), {}, null)
        const body = this.parseBody(source)
        return new If(cond, body, null, source.absPos())
    }

    parseFunction(source) {
        const args = this.readArguments(source)
        const body = this.parseBody(source)
        return new LangFunction(body, args, source.absPos())
    }

    readArguments(source) {
        consumeSpaces(source)

        if ('(' !== source.currentChar()) {
            throw new LangParseError(Errors.EXPECTED_SYMBOL, source.absPos(), '(', source.currentChar())
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
            throw new LangParseError(Errors.EXPECTED_SYMBOL, source.absPos(), ')', source.currentChar())
        }
        source.move()
        return args
    }

    parseBody(source) {
        consumeSpaces(source)

        if ('{' !== source.currentChar()) {
            throw new LangParseError(Errors.EXPECTED_SYMBOL, source.absPos(), '{', source.currentChar())
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
            throw new LangParseError(Errors.EXPECTED_SYMBOL, source.absPos(), '}', source.currentChar())
        }
        source.move()

        if (/^\s*$/.test(body)) {
            return new Block([new LangVoid(source.absPos())], source.absPos())
        }

        const block = this.parseBlock(new Source(body, source.absPos() - body.length))
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
        throw new LangParseError(Errors.UNEXPECTED_END, source.absPos())
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
        throw new LangParseError(Errors.EXPECTED_IDENTIFIER, source.absPos())
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
                if (curlies < 0) throw new LangParseError(Errors.UNEXPECTED_SYMBOL, source.absPos(), c)
            }
            token += c
        }
        if (token) return token
    }

    _stepper() {
        this.steps++
        if (this.steps > this.maxSteps) throw new LangParseError(Errors.PARSER_STEPS_EXCEEDED, source.absPos())
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

/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	
/******/ 	// startup
/******/ 	// Load entry module and return exports
/******/ 	// This entry module is referenced by other modules so it can't be inlined
/******/ 	var __webpack_exports__ = __webpack_require__("./src/ludolfc.js");
/******/ 	
/******/ 	return __webpack_exports__;
/******/ })()
;
});
//# sourceMappingURL=ludolfc.js.map