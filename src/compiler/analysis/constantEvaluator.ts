import {
    BinaryOperation,
    BoolType,
    Expression,
    Literal,
    RationalNumberType,
    Type,
    UnaryOperation
} from "../ast/ast";
import { ASTVisitor } from "../ast/astVisitor";
import { DiagnosticReporter } from "../interface/diagnosticReporter";
import { isCompareOp } from "../parsing/token";

/**
 * Small drop-in replacement for TypeChecker to evaluate simple expressions of integer constants.
 */
export class ConstantEvaluator extends ASTVisitor {
    constructor(expr: Expression, private readonly diagnosticReporter: DiagnosticReporter) {
        super();
        expr.accept(this);
    }

    public endVisitUnaryOperation(operation: UnaryOperation) {
        const subType = operation.subExpression.annotation.type;
        if (!(subType instanceof RationalNumberType))
            this.diagnosticReporter.fatalTypeError("Invalid constant expression.", operation.subExpression.location);
        const t = subType.unaryOperatorResult(operation.operator);
        operation.annotation.type = t;
    }

    public endVisitBinaryOperation(operation: BinaryOperation) {
        const leftType = operation.leftExpression.annotation.type;
        const rightType = operation.rightExpression.annotation.type;
        if (!(leftType instanceof RationalNumberType))
            this.diagnosticReporter.fatalTypeError("Invalid constant expression.", operation.leftExpression.location);
        if (!(rightType instanceof RationalNumberType))
            this.diagnosticReporter.fatalTypeError("Invalid constant expression.", operation.rightExpression.location);
        let commonType = leftType.binaryOperatorResult(operation.operator, rightType);
        if (isCompareOp(operation.operator))
            commonType = new BoolType();
        operation.annotation.type = commonType;
    }

    public endVisitLiteral(literal: Literal) {
        literal.annotation.type = Type.forLiteral(literal);
        if (!literal.annotation.type)
            this.diagnosticReporter.fatalTypeError("Invalid literal value.", literal.location);
    }
}
