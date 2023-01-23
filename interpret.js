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
    LangError,
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
        let result = null
        for (let stm of block.statements) {
            const r = this.executeStatement(stm)
            if (r) result = r
        }
        return result
    }

    executeStatement(stm) {
        return stm.isExpression ? this.executeExpression(stm) :
               stm.isAssignment ? this.executeAssignemt(stm) :
               stm.isWhile ? this.executeWhile(stm) :
               stm.isIf ? this.executeIf(stm) : 
               null
    }

    executeExpression(expression, assignNewValue = null) {
        if (!expression.parts) throw new Error('Empty expression') // TODO
        let parts = expression.parts
        let index
        while ((index = findNextOp()) > -1) {
            const op = parts[index]

            if (assignNewValue && !op.isAccess) throw new Error('Access operator expected') // TODO

            if (op.isUni) {
                const a = this.executeExpressionPart(parts[index + 1])
                if (!a.type) throw new Error('Wrong subject') // TODO
                parts[index] = op.apply(a)
                parts = removeElementAt(parts, index + 1)
            } else 
            if (op.isBi) {
                const a = this.executeExpressionPart(parts[index - 1])
                const b = this.executeExpressionPart(parts[index + 1])
                if (!a.type || !b.type) throw new Error('Wrong subjects') // TODO
                parts[index] = op.apply(a, b)
                parts = removeElementAt(parts, index - 1, index + 1)
            } else 
            if (op.isArrayAccess) {
                const a = this.executeExpressionPart(parts[index - 1])
                if (Types.ARRAY !== a.type) throw new Error('Not an array') // TODO
                const indexes = op.indexes.map(i => this.executeExpressionPart(i))
                parts[index] = op.apply(a, indexes, (assignNewValue && isLastOperator()) ? assignNewValue : null)
                parts = removeElementAt(parts, index - 1)
            } else 
            if (op.isObjectAccess) {
                const o = this.executeExpressionPart(parts[index - 1])
                if (!o.isObject) throw new Error('Not an object') // TODO
                parts[index] = op.apply(o, (assignNewValue && isLastOperator()) ? assignNewValue : null)
                parts = removeElementAt(parts, index - 1)
            } else 
            if (op.isCall) {
                // TODO
                throw new Error('func call not implemented yet')
            }
            else throw new Error('Operator unknown') // TODO
        }
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
            if (!this.variables.has(expressionPart.varName)) throw new Error('Unreferenced variable') // TODO
            return this.variables.get(expressionPart.varName)
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
            }
        }
        return expressionPart
    }

    executeAssignemt(assignment) {
        if (!assignment.left || !assignment.right) throw new Error('Wrong assignement') // TODO
        const value = this.executeExpressionPart(assignment.right)
        if (assignment.left.isVariable) {
            this.variables.set(assignment.left.name, value)
        } else
        if (assignment.left.isExpression) {
            this.executeExpression(assignment.left, value)
        }
        else throw new Error('Wrong assignement left type') // TODO
    }

    executeWhile(whileStm) {

    }

    executeIf(ifStm) {
        
    }

}

module.exports = Interpret