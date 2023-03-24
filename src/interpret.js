const { 
    Types,
    Errors,
    Interruptions,
    InterpretError: LangInterpretError,
    Interrupt: LangInterrupt,
    Void: LangVoid } = require('./lang')
    
class Interpret {
    constructor(imports = {}, controls, maxSteps = 100000) {
        this.imports = imports
        this.stepper = new ExecutionStepper(maxSteps, controls && controls.isInterrupted) // to prevent infinite loops        
    }

    async execute(ast) {
        this.variables = new VariablesScope(this.imports)
        this.stepper.reset()
        return await this.executeBlock(ast, false)
    }

    async executeBlock(block, newScope = true) {        
        if (newScope) this.variables = this.variables.pushScope()
        let result
        for (let stm of block.statements) {
            result = await this.executeStatement(stm)
        }
        if (newScope) this.variables = this.variables.popScope()
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
                    if (a.type !== b.type && !(op.isAddition && Types.STRING === a.type && b.isPrimitive)) throw new LangInterpretError(Errors.UNMATCHING_BI_OPERATOR_SUBJECTS, op.source)
                    parts[index] = op.apply(a, b)
                    parts = removeElementAt(parts, index - 1, index + 1)
                } else
                if (op.isArrayAccess) {
                    const a = await this.executeExpressionPart(parts[index - 1])
                    if (Types.ARRAY !== a.type) throw new LangInterpretError(Errors.EXPECTED_ARRAY, op.source)
                    if (assignNewValue && a.protectedAttributes()) throw new LangInterpretError(Errors.PROTECTED_FROM_MODIFICATION, op.source)
                    const indexes = await Promise.all(op.indexes.map(i => this.executeExpressionPart(i)))
                    parts[index] = op.apply(a, indexes, (assignNewValue && isLastOperator()) ? assignNewValue : null)
                    if (!parts[index]) throw new LangInterpretError(Errors.ATTRIBUTE_NOT_FOUND, op.source)
                    parts = removeElementAt(parts, index - 1)
                    assignApplied = true
                } else
                if (op.isObjectAccess) {
                    const o = await this.executeExpressionPart(parts[index - 1])
                    if (!o.isObject && !o.isFunction) throw new LangInterpretError(Errors.EXPECTED_OBJECT, op.source)
                    if (assignNewValue && o.protectedAttributes()) throw new LangInterpretError(Errors.PROTECTED_FROM_MODIFICATION, op.source)
                    parts[index] = op.apply(o, (assignNewValue && isLastOperator()) ? assignNewValue : null)
                    if (!parts[index]) throw new LangInterpretError(Errors.ATTRIBUTE_NOT_FOUND, op.source)
                    parts = removeElementAt(parts, index - 1)
                    assignApplied = true
                } else 
                if (op.isCall) {
                    parts[index] = new FunctionExecution(parts[index - 1], op.params)  // to prevent immediate execution
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
        if (expressionPart.isFunctionExecution) {
            const func = await this.executeExpressionPart(expressionPart.funcExp)
            const params = await Promise.all(expressionPart.params.map(p => this.executeExpressionPart(p)))
            return await this.executeFunctionCall(func, params)
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
        if (expressionPart.isFunction && !expressionPart.scope) {
            expressionPart.scope = this.variables.copy()            
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
        const variablesBak = this.variables
        this.variables = f.scope.pushScope()
        for (let arg of f.args) {
            this.variables.setVariable(arg, params[i++], true)
        }
        if (f.parent) {
            // cache "this" object into variable $
            this.variables.setVariable('$', f.parent, true)
        }
        
        try {
            const body = f.body.copy()
            const result = await this.executeBlock(body, false)
            return result

        } finally {  // clean up variables
            this.variables.popScope()
            this.variables = variablesBak
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
        this.variables = new Map()
        this.parent = null
        if (imports) Object.entries(imports).forEach(([k,v]) => this.variables.set(k, v))
    }

    hasVariable(name) {
        if (this.variables.has(name)) return true
        if (this.variables.has('$')) {
            const self = this.variables.get('$')
            if (self.hasAttribute(name)) return true
        }
        if (this.parent) return this.parent.hasVariable(name)
        return false
    }

    getVariable(name) {
        if (this.variables.has(name)) return this.variables.get(name)
        if (this.variables.has('$')) {
            const self = this.variables.get('$')
            if (self.hasAttribute(name)) return self.attribute(name)
        }
        if (this.parent) return this.parent.getVariable(name)
        return false
    }

    setVariable(name, value, scoped = false) {
        if (scoped) {
            this.variables.set(name, value)
            return
        }
        let found = false
        let scope = this
        do {
            if (scope.variables.has(name)) {
                scope.variables.set(name, value)
                found = true
            }
            scope = scope.parent
        } while (!found && scope)
        
        if (!found) {
            this.variables.set(name, value)
        }
    }

    pushScope() {
        const newScope = new VariablesScope()
        newScope.parent = this
        return newScope
    }

    popScope() {
        const parent = this.parent
        this.parent = null
        return parent
    }

    copy() {
        const newScope = new VariablesScope(this.imports)
        newScope.variables = this.variables
        newScope.parent = this.parent
        return newScope
    }
}

// wrapper over a function call (func and params are not resolved yet)
class FunctionExecution {
    constructor(funcExp, params) {
        this.funcExp = funcExp
        this.params = params
        this.isFunctionExecution = true
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