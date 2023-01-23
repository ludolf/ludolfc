const { 
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
    LangInterpretError,
    LangObject,
    LangNumber,
    LangString,
    LangBoolean,
    LangArray,
    LangFunction,
    LangVoid } = require('./lang')

const Parser = require('./parser')

class Interpret {
    constructor() {
        this.variables = new Map()
    }

    execute(ast) {
        this.variables.clear()
        return this.executeBlock(ast)
    }

    executeBlock(block) {
        let result
        for (let stm of block.statements) {
            result = this.executeStatement(stm)
        }
        return result ? result : new LangVoid()
    }

    executeStatement(stm) {
        return stm.isExpression ? this.executeExpression(stm) :
               stm.isAssignment ? this.executeAssignemt(stm) :
               stm.isWhile ? this.executeWhile(stm) :
               stm.isIf ? this.executeIf(stm) : 
               stm
    }

    executeExpression(expression, assignNewValue = null) {
        if (!expression.parts) throw new LangInterpretError(Errors.EMPTY_EXPRESSION)
        let parts = expression.parts
        let index
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
    }

    executeExpressionPart(expressionPart) {
        if (expressionPart.isReference) {
            if (!this.hasVariable(expressionPart.varName)) throw new LangInterpretError(Errors.UNREFERENCED_VARIABLE)
            return this.getVariable(expressionPart.varName)
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
                if (obj[k].isObject) obj[k].parent = expressionPart
            }
        }
        return expressionPart
    }

    executeFunctionCall(f, params) {
        if (f.native) {
            return f.value(...params)
        }

        if ((!params && f.args) || params.length !== f.args.length) throw new LangInterpretError(Errors.FUNC_ARGUMENTS_MISHMASH)
        // cache scoped variables
        const cache = {}
        let i = 0
        for (let arg of f.args) {
            if (this.variables.has(arg)) cache[arg] = this.variables.get(arg)
            this.variables.set(arg, params[i++])
        }
        if (f.parent) {
            // cache "this" object into variable $
            if (this.variables.has('$')) cache['$'] = this.variables.get('$')
            this.variables.set('$', f.parent)
        }
        
        try {
            const result = this.executeBlock(f.value)
            return result

        } finally {  // clean up variables
            if (cache['$']) this.variables.set('$', cache['$'])
            else this.variables.delete('$')

            for (let arg of f.args) {
                if (cache[arg]) this.variables.set(arg, cache[arg])
                else this.variables.delete(arg)
            }
        }
    }

    executeAssignemt(assignment) {
        if (!assignment.left || !assignment.right) throw new LangInterpretError(Errors.WRONG_ASSIGNMENT)
        const value = this.executeExpressionPart(assignment.right)
        if (assignment.left.isVariable) {
            this.variables.set(assignment.left.name, value)
        } else
        if (assignment.left.isExpression) {
            this.executeExpression(assignment.left, value)
        }
        else throw new LangInterpretError(Errors.WRONG_ASSIGNEE_TYPE)
    }

    executeWhile(whileStm) {

    }

    executeIf(ifStm) {
        
    }

    hasVariable(name) {
        if (this.variables.has(name)) return true
        if (this.variables.has('$')) {
            const self = this.variables.get('$')
            if (self.hasAttribute(name)) return true
        }
        return false
    }

    getVariable(name) {
        if (this.variables.has(name)) return this.variables.get(name)
        if (this.variables.has('$')) {
            const self = this.variables.get('$')
            if (self.hasAttribute(name)) return self.attribute(name)
        }
        return false
    }

}

module.exports = Interpret