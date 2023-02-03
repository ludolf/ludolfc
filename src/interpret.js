const { 
    Types,
    Errors,
    InterpretError: LangInterpretError,
    Void: LangVoid } = require('./lang')

class Interpret {
    constructor(imports = {}) {
        this.variables = new VariablesScope(imports)
        this.steps = 0
        this.maxSteps = 100000 // to prevent infinite loops
    }

    execute(ast) {
        this.variables.clear()
        this.steps = 0
        return this.executeBlock(ast, false)
    }

    executeBlock(block, newScope = true) {        
        if (newScope) this.variables.pushScope()
        let result
        for (let stm of block.statements) {
            result = this.executeStatement(stm)
        }
        if (newScope) this.variables.popScope()
        return result ? result : new LangVoid()
    }

    executeStatement(stm) {
        this._stepper()
        return stm.isExpression ? this.executeExpression(stm) :
               stm.isAssignment ? this.executeAssignemt(stm) :
               stm.isWhile ? this.executeWhile(stm) :
               stm.isIf ? this.executeIf(stm) : 
               stm
    }

    executeExpression(expression, assignNewValue = null) {
        this._stepper()
        if (!expression.parts) throw new LangInterpretError(Errors.EMPTY_EXPRESSION)
        let parts = [...expression.parts]
        return this.executeExpressionParts(parts, assignNewValue)
    }

    executeExpressionParts(parts, assignNewValue = null) {
        // logical operators short circuit
        let index = findFirstOp('&')
        if (index) {
            const left = this.executeExpressionParts(parts.slice(0, index), assignNewValue)
            if (left.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS)
            if (!left.value) return left
            const right = this.executeExpressionParts(parts.slice(index + 1), assignNewValue)
            if (right.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS)
            return right
        }
        index = findFirstOp('|')
        if (index) {
            const left = this.executeExpressionParts(parts.slice(0, index), assignNewValue)
            if (left.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS)
            if (left.value) return left
            const right = this.executeExpressionParts(parts.slice(index + 1), assignNewValue)
            if (right.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS)
            return right
        }

        // left to right by precendence
        let assignApplied = false
        while ((index = findNextOp()) > -1) {
            const op = parts[index]

            if (assignNewValue && !op.isObjectAccess && !op.isArrayAccess) throw new LangInterpretError(Errors.ACCESS_OPERATOR_EXPECTED)

            if (op.isUni) {
                const a = this.executeExpressionPart(parts[index + 1])
                if (!a.type) throw new LangInterpretError(Errors.WRONG_UNI_OPERATOR_SUBJECT)
                parts[index] = op.apply(a)
                parts = removeElementAt(parts, index + 1)
            } else 
            if (op.isBi) {
                const a = this.executeExpressionPart(parts[index - 1])
                const b = this.executeExpressionPart(parts[index + 1])
                if (!a.type || !b.type) throw new LangInterpretError(Errors.WRONG_BI_OPERATOR_SUBJECTS)
                if (a.type !== b.type) throw new LangInterpretError(Errors.UNMATCHING_BI_OPERATOR_SUBJECTS)
                parts[index] = op.apply(a, b)
                parts = removeElementAt(parts, index - 1, index + 1)
            } else 
            if (op.isArrayAccess) {
                const a = this.executeExpressionPart(parts[index - 1])
                if (Types.ARRAY !== a.type) throw new LangInterpretError(Errors.EXPECTED_ARRAY)
                const indexes = op.indexes.map(i => this.executeExpressionPart(i))
                parts[index] = op.apply(a, indexes, (assignNewValue && isLastOperator()) ? assignNewValue : null)
                parts = removeElementAt(parts, index - 1)
                assignApplied = true
            } else 
            if (op.isObjectAccess) {
                const o = this.executeExpressionPart(parts[index - 1])
                if (!o.isObject) throw new LangInterpretError(Errors.EXPECTED_OBJECT)
                parts[index] = op.apply(o, (assignNewValue && isLastOperator()) ? assignNewValue : null)
                parts = removeElementAt(parts, index - 1)
                assignApplied = true
            } else 
            if (op.isCall) {
                const f = this.executeExpressionPart(parts[index - 1])
                const params = op.params.map(p => this.executeExpressionPart(p))
                parts[index] = this.executeFunctionCall(f, params)
                parts = removeElementAt(parts, index - 1)
            }
            else throw new LangInterpretError(Errors.UNKNOWN_OPERATOR)
        }

        if (assignNewValue && !assignApplied) throw new LangInterpretError(Errors.ACCESS_OPERATOR_EXPECTED)

        return this.executeExpressionPart(parts[0]) // parts are reduced to a single result

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

    executeExpressionPart(expressionPart) {
        this._stepper()
        if (expressionPart.isReference) {
            if (!this.variables.hasVariable(expressionPart.varName)) throw new LangInterpretError(Errors.UNREFERENCED_VARIABLE, expressionPart.varName)
            return this.variables.getVariable(expressionPart.varName)
        }
        if (expressionPart.isExpression) {
            return this.executeExpression(expressionPart)
        }
        if (Types.ARRAY === expressionPart.type) {
            const arr = expressionPart.value
            for (let i = 0; i < arr.length; i++) {
                arr[i] = this.executeExpressionPart(arr[i])
            }
        } else
        if (Types.OBJECT === expressionPart.type) {
            const obj = expressionPart.value
            for (let k of Object.keys(obj)) {
                obj[k] = this.executeExpressionPart(obj[k])
                if (obj[k].isObject || obj[k].isFunction) obj[k].parent = expressionPart
            }
        }
        return expressionPart
    }

    executeFunctionCall(f, params) {
        if (f.isNative) {
            return f.call(...params)
        }

        if ((!params && f.args) || params.length !== f.args.length) throw new LangInterpretError(Errors.FUNC_ARGUMENTS_MISHMASH)
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
            const result = this.executeBlock(f.body, false)
            return result

        } finally {  // clean up variables
            this.variables.popScope()
        }
    }

    executeAssignemt(assignment) {
        if (!assignment.left || !assignment.right) throw new LangInterpretError(Errors.WRONG_ASSIGNMENT)
        const value = this.executeExpressionPart(assignment.right)
        if (assignment.left.isVariable) {
            this.variables.setVariable(assignment.left.name, value)
        } else
        if (assignment.left.isExpression) {
            this.executeExpression(assignment.left, value)
        }
        else throw new LangInterpretError(Errors.WRONG_ASSIGNEE_TYPE)
    }

    executeWhile(whileStm) {
        if (!whileStm.condition || !whileStm.condition.isExpression) throw new LangInterpretError(Errors.WRONG_CONDITION)
        while (true) {
            const cond = this.executeExpressionPart(whileStm.condition)
            if (cond.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_CONDITION_VALUE)
            if (cond.value) this.executeBlock(whileStm.body)
            else break
        } 
    }

    executeIf(ifStm) {
        if (!ifStm.condition || !ifStm.condition.isExpression) throw new LangInterpretError(Errors.WRONG_CONDITION)
        const cond = this.executeExpressionPart(ifStm.condition)
        if (cond.type !== Types.BOOLEAN) throw new LangInterpretError(Errors.WRONG_CONDITION_VALUE)
        if (cond.value) this.executeBlock(ifStm.body)
        else if (ifStm.elseBody) this.executeBlock(ifStm.elseBody)
    }

    _stepper() {
        this.steps++
        if (this.steps > this.maxSteps) throw new LangInterpretError(Errors.EXECUTION_STEPS_EXCEEDED)
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
        if (!found) this.variables[this.variables.length - 1].set(name, value)
    }

    pushScope() {
        this.variables.push(new Map())
    }

    popScope() {
        this.variables.pop()
    }

}

module.exports = Interpret