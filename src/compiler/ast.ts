import { ASTVisitor } from "./astVisitor";
import { Debug, applyMixins } from "./core";
import {
    ElementaryTypeNameToken,
    TokenName,
    isAssignmentOp,
    isBinaryOp,
    isCompareOp,
    isUnaryOp
} from "./token";
import { SourceLocation } from "./types";

class IDDispenser {
    private id = 0;

    public next(): number { return ++this.id; }
    public reset() {
        this.id = 0;
    }
}

const idDispenser = new IDDispenser();

// How a function can mutate the EVM state.
export enum StateMutability { Pure, View, NonPayable, Payable }

export function stateMutabilityToString(stateMutability: StateMutability) {
    return StateMutability[stateMutability].toLowerCase();
}

/**
 * Abstract class that is added to each AST node that can receive documentation.
 */
export class Documented {
    /// @return A shared pointer of an ASTString.
    /// Can contain an undefined in which case indicates absence of documentation
    public documentation(): string | undefined {
        return this._documentation;
    }

    _documentation?: string;
}

export class ImplementationOptional {
    /// @return whether this node is fully implemented or not
    public isImplemented(): boolean { return this._implemented; }

    _implemented: boolean;
}

/**
 * Abstract class that is added to each AST node that can store local variables.
 */
export class VariableScope {
    public addLocalVariable(localVariable: VariableDeclaration) { this._localVariables.push(localVariable); }
    public localVariables(): ReadonlyArray<VariableDeclaration> { return this._localVariables; }

    _localVariables: VariableDeclaration[];
}

export abstract class ASTNode {
    private _id: number;

    private _location: SourceLocation;

    public get id() {
        return this._id;
    }

    public get location(): SourceLocation {
        return this._location;
    }

    constructor(location: SourceLocation) {
        this._id = idDispenser.next();
        this._location = location;
    }

    public abstract accept(visitor: ASTVisitor): void;
}

function listAccept<T extends ASTNode>(list: ReadonlyArray<T>, visitor: ASTVisitor) {
    for (const element of list)
        element.accept(visitor);
}

export class SourceUnit extends ASTNode {
    constructor(
        location: SourceLocation,
        public readonly text: string,
        public readonly nodes: ReadonlyArray<ASTNode>
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitSourceUnit(this))
            listAccept(this.nodes, visitor);
        visitor.endVisitSourceUnit(this);
    }
}

export class PragmaDirective extends ASTNode {
    constructor(
        location: SourceLocation,
        public readonly tokens: ReadonlyArray<TokenName>,
        public readonly literals: ReadonlyArray<string>
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitPragmaDirective(this);
        visitor.endVisitPragmaDirective(this);
    }
}

/**
 * An expression, i.e. something that has a value (which can also be of type "void" in case
 * of some function calls).
 * @abstract
 */
export abstract class Expression extends ASTNode {
    constructor(location: SourceLocation) {
        super(location);
    }
}

/**
 * Primary expression, i.e. an expression that cannot be divided any further. Examples are literals
 * or variable references.
 */
export abstract class PrimaryExpression extends Expression {
    constructor(location: SourceLocation) {
        super(location);
    }
}

/// Assignment, can also be a compound assignment.
/// Examples: (a = 7 + 8) or (a *= 2)
export class Assignment extends Expression {
    constructor(
        location: SourceLocation,
        public readonly leftHandSide: Expression,
        public readonly assignmentOperator: TokenName,
        public readonly rightHandSide: Expression) {
        super(location);
        Debug.assert(isAssignmentOp(assignmentOperator));
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitAssignment(this)) {
            this.leftHandSide.accept(visitor);
            this.rightHandSide.accept(visitor);
        }
        visitor.endVisitAssignment(this);
    }
}

export class Conditional extends Expression {
    constructor(
        location: SourceLocation,
        public readonly condition: Expression,
        public readonly trueExpression: Expression,
        public readonly falseExpression: Expression) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitConditional(this)) {
            this.condition.accept(visitor);
            this.trueExpression.accept(visitor);
            this.falseExpression.accept(visitor);
        }
        visitor.endVisitConditional(this);
    }
}

/**
 * Index access to an array. Example: a[2]
 */
export class IndexAccess extends Expression {
    constructor(
        location: SourceLocation,
        public readonly baseExpression: Expression,
        public readonly indexExpression: Expression | undefined
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitIndexAccess(this)) {
            this.baseExpression.accept(visitor);
            if (this.indexExpression)
                this.indexExpression.accept(visitor);
        }
        visitor.endVisitIndexAccess(this);
    }
}

/**
 * Access to a member of an object. Example: x.name
 */
export class MemberAccess extends Expression {
    constructor(
        location: SourceLocation,
        public readonly expression: Expression,
        public readonly memberName: string) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitMemberAccess(this))
            this.expression.accept(visitor);
        visitor.endVisitMemberAccess(this);
    }
}

/**
 * Expression that creates a new contract or memory-array,
 * e.g. the "new SomeContract" part in "new SomeContract(1, 2)".
 */
export class NewExpression extends Expression {
    constructor(
        location: SourceLocation,
        public readonly typeName: TypeName
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitNewExpression(this))
            this.typeName.accept(visitor);
        visitor.endVisitNewExpression(this);
    }
}

/**
 * Can be ordinary function call, type cast or struct construction.
 */
export class FunctionCall extends Expression {
    constructor(
        location: SourceLocation,
        public readonly expression: Expression,
        public readonly args: ReadonlyArray<Expression>,
        public readonly names: ReadonlyArray<string>
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitFunctionCall(this)) {
            this.expression.accept(visitor);
            listAccept(this.args, visitor);
        }
        visitor.endVisitFunctionCall(this);
    }
}

/**
 * Operation involving a unary operator, pre- or postfix.
 * Examples: ++i, delete x or !true
 */
export class UnaryOperation extends Expression {
    constructor(
        location: SourceLocation,
        public readonly operator: TokenName,
        public readonly subExpression: Expression,
        private isPrefix: boolean) {
        super(location);
        Debug.assert(isUnaryOp(operator));
    }

    public isPrefixOperation(): boolean {
        return this.isPrefix;
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitUnaryOperation(this))
            this.subExpression.accept(visitor);
        visitor.endVisitUnaryOperation(this);
    }
}

/**
 * Operation involving a binary operator.
 * Examples: 1 + 2, true && false or 1 <= 4
 */
export class BinaryOperation extends Expression {
    constructor(
        location: SourceLocation,
        public readonly leftExpression: Expression,
        public readonly operator: TokenName,
        public readonly rightExpression: Expression) {
        super(location);
        Debug.assert(isBinaryOp(operator) || isCompareOp(operator));
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitBinaryOperation(this)) {
            this.leftExpression.accept(visitor);
            this.rightExpression.accept(visitor);
        }
        visitor.endVisitBinaryOperation(this);
    }
}

/**
 * Tuple, parenthesized expression, or bracketed expression.
 * Examples: (1, 2), (x,), (x), (), [1, 2],
 * Individual components might be empty shared pointers (as in the second example).
 * The respective types in lvalue context are: 2-tuple, 2-tuple (with wildcard), type of x, 0-tuple
 * Not in lvalue context: 2-tuple, _1_-tuple, type of x, 0-tuple.
 */
export class TupleExpression extends Expression {
    constructor(
        location: SourceLocation,
        public readonly components: ReadonlyArray<Expression | undefined>,
        private _isArray: boolean
    ) {
        super(location);
    }

    public isInlineArray(): boolean {
        return this._isArray;
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitTupleExpression(this)) {
            for (const component of this.components) {
                if (component)
                    component.accept(visitor);
            }
        }
        visitor.endVisitTupleExpression(this);
    }
}

/**
 * An identifier, i.e. a reference to a declaration by name like a variable or function.
 */
export class Identifier extends PrimaryExpression {
    constructor(
        location: SourceLocation,
        public readonly name: string) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitIdentifier(this);
        visitor.endVisitIdentifier(this);
    }
}

export const enum SubDenomination {
    None = TokenName.Illegal,
    Wei = TokenName.SubWei,
    Szabo = TokenName.SubSzabo,
    Finney = TokenName.SubFinney,
    Ether = TokenName.SubEther,
    Second = TokenName.SubSecond,
    Minute = TokenName.SubMinute,
    Hour = TokenName.SubHour,
    Day = TokenName.SubDay,
    Week = TokenName.SubWeek,
    Year = TokenName.SubYear
};

/**
 * A literal string or number. @see ExpressionCompiler::endVisit() is used to actually parse its value.
 */
export class Literal extends PrimaryExpression {
    constructor(
        location: SourceLocation,
        public readonly token: TokenName,
        public readonly value: string,
        public readonly subDenomination = SubDenomination.None) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitLiteral(this);
        visitor.endVisitLiteral(this);
    }

    /// @returns true if this is a number with a hex prefix.
    public isHexNumber(): boolean {
        if (this.token !== TokenName.Number)
            return false;
        return this.value.startsWith("0x");
    }

    /// @returns true if this looks like a checksummed address.
    public looksLikeAddress(): boolean {
        if (this.subDenomination !== SubDenomination.None)
            return false;

        if (!this.isHexNumber())
            return false;

        return Math.abs(this.value.length - 42) <= 1;
    }
}

/**
 * An elementary type name expression is used in expressions like "a = uint32(2)" to change the
 * type of an expression explicitly. Here, "uint32" is the elementary type name expression and
 * "uint32(2)" is a @ref FunctionCall.
 */
export class ElementaryTypeNameExpression extends PrimaryExpression {
    constructor(
        location: SourceLocation,
        public readonly typeName: ElementaryTypeNameToken
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitElementaryTypeNameExpression(this);
        visitor.endVisitElementaryTypeNameExpression(this);
    }
}

/// Visibility ordered from restricted to unrestricted.
export enum Visibility { Default, Private, Internal, Public, External };

export function visibilityToString(visibility: Visibility): string {
    return Visibility[visibility].toLowerCase();
}

/**
 * Abstract AST class for a declaration (contract, function, struct, variable, import directive).
 */
export abstract class Declaration extends ASTNode {
    private _scope?: ASTNode;

    constructor(
        location: SourceLocation,
        public readonly name: string,
        private _visibility = Visibility.Default) {
        super(location);
    }

    public noVisibilitySpecified(): boolean {
        return this._visibility === Visibility.Default;
    }
    public get visibility(): Visibility {
        return this._visibility === Visibility.Default ? this.defaultVisibility : this._visibility;
    }
    public isPublic(): boolean {
        return this.visibility >= Visibility.Public;
    }
    public isVisibleInContract(): boolean {
        return this.visibility !== Visibility.External;
    }
    public isVisibleInDerivedContracts(): boolean {
        return this.isVisibleInContract() && this.visibility >= Visibility.Internal;
    }
    protected get defaultVisibility(): Visibility {
        return Visibility.Public;
    }

    /// @returns the scope this declaration resides in. Can be undefined if it is the global scope.
    /// Available only after name and type resolution step.
    public get scope(): ASTNode {
        return this._scope;
    }
    public setScope(scope: ASTNode) {
        this._scope = scope;
    }

    /// @returns the source unit this declaration is present in.
    public get sourceUnit(): SourceUnit {
        Debug.assert(!!this._scope);
        let scope = this._scope;
        while (scope instanceof Declaration && scope._scope instanceof Declaration)
            scope = scope._scope;
        return scope as SourceUnit;
    }

    public isLValue(): boolean {
        return false;
    }
    public isPartOfExternalInterface(): boolean {
        return false;
    }
}

/**
 * Base class for all nodes that define function-like objects, i.e. FunctionDefinition,
 * EventDefinition and ModifierDefinition.
 */
export abstract class CallableDeclaration extends Declaration implements VariableScope {
    constructor(
        location: SourceLocation,
        name: string,
        visibility: Visibility,
        public readonly parameterList: ParameterList,
        public readonly returnParameterList: ParameterList | undefined) {
        super(location, name, visibility);
    }

    public get parameters(): ReadonlyArray<VariableDeclaration> {
        return this.parameterList.parameters;
    }

    public addLocalVariable: (localVariable: VariableDeclaration) => void;
    public localVariables: () => ReadonlyArray<VariableDeclaration>;

    _localVariables: VariableDeclaration[] = [];
}
applyMixins(CallableDeclaration, [VariableScope]);


export class FunctionDefinition extends CallableDeclaration implements Documented, ImplementationOptional {
    constructor(
        location: SourceLocation,
        name: string,
        visibility: Visibility,
        public readonly stateMutability: StateMutability,
        private _isConstructor: boolean,
        documentation: string,
        parameters: ParameterList,
        public readonly modifiers: ReadonlyArray<ModifierInvocation>,
        returnParameters: ParameterList | undefined,
        public readonly body: Block | undefined) {
        super(location, name, visibility, parameters, returnParameters);
        this._documentation = documentation;
        this._implemented = !!body;
    }

    public documentation: () => string;
    _documentation: string;

    /// @return whether this node is fully implemented or not
    public isImplemented(): boolean { return this._implemented; }
    _implemented: boolean;

    public accept(visitor: ASTVisitor) {
        if (visitor.visitFunctionDefinition(this)) {
            this.parameterList.accept(visitor);
            if (this.returnParameterList)
                this.returnParameterList.accept(visitor);
            listAccept(this.modifiers, visitor);
            if (this.body)
                this.body.accept(visitor);
        }
        visitor.endVisitFunctionDefinition(this);
    }

    public isConstructor(): boolean { return this._isConstructor; }
    public isFallback(): boolean { return this.name === ""; }
    public isPayable(): boolean { return this.stateMutability === StateMutability.Payable; }

    public isVisibleInContract(): boolean {
        return super.isVisibleInContract() && !this.isConstructor() && !this.isFallback();
    }
    public isPartOfExternalInterface(): boolean {
        return this.isPublic() && !this.isConstructor() && !this.isFallback();
    }
}
applyMixins(FunctionDefinition, [Documented, ImplementationOptional]);

export const enum Location { Default, Storage, Memory }

/**
 * Declaration of a variable. This can be used in various places, e.g. in function parameter
 * lists, struct definitions and even function bodies.
 */
export class VariableDeclaration extends Declaration {
    constructor(
        sourceLocation: SourceLocation,
        public readonly typeName: TypeName | undefined, ///< can be empty ("var")
        name: string,
        /// Initially assigned value, can be missing. For local variables, this is stored inside
        /// VariableDeclarationStatement and not here.
        public readonly value: Expression | undefined,
        visibility: Visibility,
        private _isStateVariable = false, ///< Whether or not this is a contract state variable
        private _isIndexed = false, ///< Whether this is an indexed variable (used by events).
        private _isConstant = false, ///< Whether the variable is a compile-time constant.
        public readonly referenceLocation = Location.Default ///< Location of the variable if it is of reference type.
    ) {
        super(sourceLocation, name, visibility);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitVariableDeclaration(this)) {
            if (this.typeName)
                this.typeName.accept(visitor);
            if (this.value)
                this.value.accept(visitor);
        }
        visitor.endVisitVariableDeclaration(this);
    }

    public isLValue(): boolean {
        // External function parameters and constant declared variables are Read-Only
        return !this.isExternalCallableParameter() && !this._isConstant;
    }

    public isPartOfExternalInterface(): boolean {
        return this.isPublic();
    }

    public isLocalVariable(): boolean {
        return !!(this.scope as CallableDeclaration);
    }

    /// @returns true if this variable is a parameter or return parameter of a function.
    public isCallableParameter(): boolean {
        const callable = this.scope as CallableDeclaration;
        if (!callable)
            return false;
        for (const variable of callable.parameters) {
            if (variable === this)
                return true;
        }
        if (callable.returnParameterList) {
            for (const variable of callable.returnParameterList.parameters) {
                if (variable === this)
                    return true;
            }
        }
        return false;
    }

    /// @returns true if this variable is a return parameter of a function.
    public isReturnParameter(): boolean {
        const callable = this.scope as CallableDeclaration;
        if (!callable)
            return false;
        if (callable.returnParameterList) {
            for (const variable of callable.returnParameterList.parameters) {
                if (variable === this)
                    return true;
            }
        }
        return false;
    }

    /// @returns true if this variable is a local variable or return parameter.
    public isLocalOrReturn(): boolean {
        return this.isReturnParameter() || (this.isLocalVariable() && !this.isCallableParameter());
    }

    /// @returns true if this variable is a parameter (not return parameter) of an external function.
    public isExternalCallableParameter(): boolean {
        const callable = this.scope as CallableDeclaration;
        if (!callable || callable.visibility !== Visibility.External)
            return false;
        for (const variable of callable.parameters) {
            if (variable === this)
                return true;
        }
        return false;
    }

    /// @returns true if the type of the variable does not need to be specified, i.e. it is declared
    /// in the body of a function or modifier.
    public canHaveAutoType(): boolean {
        const callable = this.scope as CallableDeclaration;
        return (!!callable && !this.isCallableParameter());
    }

    public isStateVariable(): boolean {
        return this._isStateVariable;
    }
    public isIndexed(): boolean {
        return this._isIndexed;
    }
    public isConstant(): boolean {
        return this._isConstant;
    }

    protected get defaultVisibility(): Visibility {
        return Visibility.Internal;
    }
}

/**
 * Invocation/usage of a modifier in a function header or a base constructor call.
 */
export class ModifierInvocation extends ASTNode {
    constructor(
        location: SourceLocation,
        public readonly name: Identifier,
        public readonly args: ReadonlyArray<Expression>) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitModifierInvocation(this)) {
            this.name.accept(visitor);
            listAccept(this.args, visitor);
        }
        visitor.endVisitModifierInvocation(this);
    }
}

/**
 * Parameter list, used as function parameter list and return list.
 * None of the parameters is allowed to contain mappings (not even recursively
 * inside structs).
 */
export class ParameterList extends ASTNode {
    constructor(
        location: SourceLocation,
        public readonly parameters: ReadonlyArray<VariableDeclaration>
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitParameterList(this))
            listAccept(this.parameters, visitor);
        visitor.endVisitParameterList(this);
    }
}

export class InheritanceSpecifier extends ASTNode {
    constructor(
        location: SourceLocation,
        public readonly name: UserDefinedTypeName,
        public readonly args: ReadonlyArray<Expression>) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitInheritanceSpecifier(this)) {
            this.name.accept(visitor);
            listAccept(this.args, visitor);
        }
        visitor.endVisitInheritanceSpecifier(this);
    }
}

/**
 * Abstract base class for statements.
 */
export abstract class Statement extends ASTNode implements Documented {
    constructor(
        location: SourceLocation,
        docString: string) {
        super(location);
        this._documentation = docString;
    }

    public documentation: () => string;
    _documentation: string;
}
applyMixins(Statement, [Documented]);

/**
 * Brace-enclosed block containing zero or more statements.
 */
export class Block extends Statement {
    constructor(
        location: SourceLocation,
        docString: string,
        public readonly statements: ReadonlyArray<Statement>) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitBlock(this))
            listAccept(this.statements, visitor);
        visitor.endVisitBlock(this);
    }
}

/**
 * Special placeholder statement denoted by "_" used in function modifiers. This is replaced by
 * the original function when the modifier is applied.
 */
export class PlaceholderStatement extends Statement {
    constructor(
        location: SourceLocation,
        docString: string) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitPlaceholderStatement(this);
        visitor.endVisitPlaceholderStatement(this);
    }
}

/**
 * If-statement with an optional "else" part. Note that "else if" is modeled by having a new
 * if-statement as the false (else) body.
 */
export class IfStatement extends Statement {
    constructor(
        location: SourceLocation,
        docString: string,
        public readonly condition: Expression,
        public readonly trueStatement: Expression,
        public readonly falseStatement: Expression | undefined) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitIfStatement(this)) {
            this.condition.accept(visitor);
            this.trueStatement.accept(visitor);
            if (this.falseStatement)
                this.falseStatement.accept(visitor);
        }
        visitor.endVisitIfStatement(this);
    }
}

/**
 * Statement in which a break statement is legal (abstract class).
 */
export abstract class BreakableStatement extends Statement {
    constructor(
        location: SourceLocation,
        docString: string) {
        super(location, docString);
    }
}

export class WhileStatement extends BreakableStatement {
    constructor(
        location: SourceLocation,
        docString: string,
        public readonly condition: Expression,
        public readonly body: Expression,
        private readonly _isDoWhile: boolean
    ) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitWhileStatement(this)) {
            this.condition.accept(visitor);
            this.body.accept(visitor);
        }
        visitor.endVisitWhileStatement(this);
    }

    public isDoWhile(): boolean {
        return this._isDoWhile;
    }
}

/**
 * For loop statement
 */
export class ForStatement extends BreakableStatement {
    constructor(
        location: SourceLocation,
        docString: string,
        public readonly initializationExpression: Expression | undefined,
        public readonly condition: Expression | undefined,
        public readonly loopExpression: Expression | undefined,
        public readonly body: Statement) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitForStatement(this)) {
            if (this.initializationExpression)
                this.initializationExpression.accept(visitor);
            if (this.condition)
                this.condition.accept(visitor);
            if (this.loopExpression)
                this.loopExpression.accept(visitor);
            this.body.accept(visitor);
        }
        visitor.endVisitForStatement(this);
    }
}

export class Continue extends Statement {
    constructor(location: SourceLocation, docString: string) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitContinue(this);
        visitor.endVisitContinue(this);
    }
}

export class Break extends Statement {
    constructor(location: SourceLocation, docString: string) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitBreak(this);
        visitor.endVisitBreak(this);
    }
}

export class Return extends Statement {
    constructor(
        location: SourceLocation,
        docString: string,
        public readonly expression: Expression | undefined) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitReturn(this)) {
            if (this.expression)
                this.expression.accept(visitor);
        }
        visitor.endVisitReturn(this);
    }
}

/**
 * @brief The Throw statement to throw that triggers a solidity exception(jump to ErrorTag)
 */
export class Throw extends Statement {
    constructor(location: SourceLocation, docString: string) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitThrow(this);
        visitor.endVisitThrow(this);
    }
}

/**
 * Definition of a variable as a statement inside a function. It requires a type name (which can
 * also be "var") but the actual assignment can be missing.
 * Examples: var a = 2; uint256 a;
 * As a second form, multiple variables can be declared, cannot have a type and must be assigned
 * right away. If the first or last component is unnamed, it can "consume" an arbitrary number
 * of components.
 * Examples: var (a, b) = f(); var (a,,,c) = g(); var (a,) = d();
 */
export class VariableDeclarationStatement extends Statement {
    constructor(
        location: SourceLocation,
        docString: string,
        public readonly declarations: ReadonlyArray<VariableDeclaration | undefined>,
        public readonly initialValue: Expression | undefined) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitVariableDeclarationStatement(this)) {
            for (const varDecl of this.declarations) {
                if (varDecl)
                    varDecl.accept(visitor);
            }
            if (this.initialValue)
                this.initialValue.accept(visitor);
        }
        visitor.endVisitVariableDeclarationStatement(this);
    }
}

/**
 * A statement that contains only an expression (i.e. an assignment, function call, ...).
 */
export class ExpressionStatement extends Statement {
    constructor(
        location: SourceLocation,
        docString: string,
        public readonly expression: Expression | undefined) {
        super(location, docString);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitExpressionStatement(this)) {
            if (this.expression)
                this.expression.accept(visitor);
        }
        visitor.endVisitExpressionStatement(this);
    }
}

export const enum ContractKind { Interface, Contract, Library }

function filteredNodes<T extends ASTNode>(cons: Function, nodes: ReadonlyArray<ASTNode>): ReadonlyArray<T> {
    const ret: T[] = [];
    for (const node of nodes) {
        if (node instanceof cons) {
            ret.push(node as T);
        }
    }
    return ret;
}

/**
 * Definition of a contract or library. This is the only AST nodes where child nodes are not visited in
 * document order. It first visits all struct declarations, then all variable declarations and
 * finally all function declarations.
 */
export class ContractDefinition extends Declaration implements Documented {
    private _inheritableMembers: Declaration[];

    constructor(
        location: SourceLocation,
        name: string,
        documentation: string,
        public readonly baseContracts: ReadonlyArray<InheritanceSpecifier>,
        public readonly subNodes: ReadonlyArray<ASTNode>,
        public readonly contractKind = ContractKind.Contract
    ) {
        super(location, name);
        this._documentation = documentation;
    }

    public documentation: () => string;
    _documentation: string;

    public accept(visitor: ASTVisitor) {
        if (visitor.visitContractDefinition(this)) {
            listAccept(this.baseContracts, visitor);
            listAccept(this.subNodes, visitor);
        }
        visitor.endVisitContractDefinition(this);
    }

    public usingForDirectives(): ReadonlyArray<UsingForDirective> {
        return filteredNodes(UsingForDirective, this.subNodes);
    }
    public definedStructs(): ReadonlyArray<StructDefinition> {
        return filteredNodes(StructDefinition, this.subNodes);
    }
    public definedEnums(): ReadonlyArray<EnumDefinition> {
        return filteredNodes(EnumDefinition, this.subNodes);
    }
    public stateVariables(): ReadonlyArray<VariableDeclaration> {
        return filteredNodes(VariableDeclaration, this.subNodes);
    }
    public functionModifiers(): ReadonlyArray<ModifierDefinition> {
        return filteredNodes(ModifierDefinition, this.subNodes);
    }
    public definedFunctions(): ReadonlyArray<FunctionDefinition> {
        return filteredNodes(FunctionDefinition, this.subNodes);
    }
    public events(): ReadonlyArray<EventDefinition> {
        return filteredNodes(EventDefinition, this.subNodes);
    }

    public isLibrary(): boolean {
        return this.contractKind === ContractKind.Library;
    }

    /// @returns a list of the inheritable members of this contract
    public inheritableMembers(): ReadonlyArray<Declaration> {
        if (!this._inheritableMembers) {
            const memberSeen = new Set<string>();
            this._inheritableMembers = [];
            const addInheritableMember = (decl: Declaration) => {
                Debug.assert(!!decl, "addInheritableMember got a undefined.");
                if (!memberSeen.has(decl.name) && decl.isVisibleInDerivedContracts()) {
                    memberSeen.add(decl.name);
                    this._inheritableMembers.push(decl);
                }
            };

            for (const f of this.definedFunctions())
                addInheritableMember(f);

            for (const v of this.stateVariables())
                addInheritableMember(v);

            for (const s of this.definedStructs())
                addInheritableMember(s);

            for (const e of this.definedEnums())
                addInheritableMember(e);

            for (const e of this.events())
                addInheritableMember(e);
        }
        return this._inheritableMembers;
    }

    /// Returns the constructor or undefined if no constructor was specified.
    public constructorFunction(): FunctionDefinition | undefined {
        for (const f of this.definedFunctions()) {
            if (f.isConstructor())
                return f;
        }
    }

    /// @returns true iff the constructor of this contract is public (or non-existing).
    public constructorIsPublic(): boolean {
        const f = this.constructorFunction();
        return !f || f.isPublic();
    }
}
applyMixins(ContractDefinition, [Documented]);

/**
 * Import directive for referencing other files / source objects.
 * Example: import "abc.sol" // imports all symbols of "abc.sol" into current scope
 * Source objects are identified by a string which can be a file name but does not have to be.
 * Other ways to use it:
 * import "abc" as x; // creates symbol "x" that contains all symbols in "abc"
 * import * as x from "abc"; // same as above
 * import {a as b, c} from "abc"; // creates new symbols "b" and "c" referencing "a" and "c" in "abc", respectively.
 */
export class ImportDirective extends Declaration {
    constructor(
        location: SourceLocation,
        public readonly path: string,
        unitAlias: string,
        /// The aliases for the specific symbols to import. If non-empty import the specific symbols.
        /// If the second component is empty, import the identifier unchanged.
        /// If both _unitAlias and _symbolAlias are empty, import all symbols into the current scope.
        public readonly symbolAliases: ReadonlyArray<[Identifier, string]>
    ) {
        super(location, unitAlias);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitImportDirective(this);
        visitor.endVisitImportDirective(this);
    }
}

/**
 * Definition of a (loggable) event.
 */
export class EventDefinition extends CallableDeclaration implements Documented {
    constructor(
        location: SourceLocation,
        name: string,
        documentation: string,
        parameters: ParameterList,
        private anonymous = false) {
        super(location, name, Visibility.Default, parameters, undefined);
        this._documentation = documentation;
    }

    public documentation: () => string;
    _documentation: string;

    public accept(visitor: ASTVisitor) {
        if (visitor.visitEventDefinition(this))
            this.parameterList.accept(visitor);
        visitor.endVisitEventDefinition(this);
    }

    public isAnonymous(): boolean { return this.anonymous; }
}
applyMixins(EventDefinition, [Documented]);

/**
 * Definition of a function modifier.
 */
export class ModifierDefinition extends CallableDeclaration implements Documented {
    constructor(
        location: SourceLocation,
        name: string,
        documentation: string,
        parameters: ParameterList,
        public readonly body: Block) {
        super(location, name, Visibility.Internal, parameters, undefined);
        this._documentation = documentation;
    }

    public documentation: () => string;
    _documentation: string;

    public accept(visitor: ASTVisitor) {
        if (visitor.visitModifierDefinition(this)) {
            this.parameterList.accept(visitor);
            this.body.accept(visitor);
        }
        visitor.endVisitModifierDefinition(this);
    }
}

/**
 * `using LibraryName for uint` will attach all functions from the library LibraryName
 * to `uint` if the first parameter matches the type. `using LibraryName for *` attaches
 * the function to any matching type.
 */
export class UsingForDirective extends ASTNode {
    constructor(
        location: SourceLocation,
        public readonly libraryName: UserDefinedTypeName,
        public readonly typeName: TypeName | undefined
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitUsingForDirective(this)) {
            this.libraryName.accept(visitor);
            if (this.typeName)
                this.typeName.accept(visitor);
        }
        visitor.endVisitUsingForDirective(this);
    }
}

export class StructDefinition extends Declaration {
    constructor(
        location: SourceLocation,
        name: string,
        public readonly members: ReadonlyArray<VariableDeclaration>) {
        super(location, name);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitStructDefinition(this)) {
            listAccept(this.members, visitor);
        }
        visitor.endVisitStructDefinition(this);
    }
}

export class EnumDefinition extends Declaration {
    constructor(
        location: SourceLocation,
        name: string,
        public readonly members: ReadonlyArray<EnumValue>
    ) {
        super(location, name);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitEnumDefinition(this)) {
            listAccept(this.members, visitor);
        }
        visitor.endVisitEnumDefinition(this);
    }
}

/**
 * Declaration of an Enum Value
 */
export class EnumValue extends Declaration {
    constructor(location: SourceLocation, name: string) {
        super(location, name);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitEnumValue(this);
        visitor.endVisitEnumValue(this);
    }
}

/**
 * Abstract base class of a type name, can be any built-in or user-defined type.
 */
export abstract class TypeName extends ASTNode {
    constructor(location: SourceLocation) {
        super(location);
    }
}

/**
 * Any pre-defined type name represented by a single keyword, i.e. it excludes mappings,
 * contracts, functions, etc.
 */
export class ElementaryTypeName extends TypeName {
    constructor(
        location: SourceLocation,
        public readonly typeName: ElementaryTypeNameToken
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitElementaryTypeName(this);
        visitor.endVisitElementaryTypeName(this);
    }
}

/**
 * An array type, can be "typename[]" or "typename[<expression>]".
 */
export class ArrayTypeName extends TypeName {
    constructor(
        location: SourceLocation,
        public readonly baseType: TypeName,
        public readonly length: Expression | undefined
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitArrayTypeName(this)) {
            this.baseType.accept(visitor);
            if (this.length)
                this.length.accept(visitor);
        }
        visitor.endVisitArrayTypeName(this);
    }
}

/**
 * A mapping type. Its source form is "mapping('keyType' => 'valueType')"
 */
export class Mapping extends TypeName {
    constructor(
        location: SourceLocation,
        public readonly keyType: ElementaryTypeName,
        public readonly valueType: TypeName) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitMapping(this)) {
            this.keyType.accept(visitor);
            this.valueType.accept(visitor);
        }
        visitor.endVisitMapping(this);
    }
}

/**
 * A literal function type. Its source form is "function (paramType1, paramType2) internal / external returns (retType1, retType2)"
 */
export class FunctionTypeName extends TypeName {
    constructor(
        location: SourceLocation,
        public readonly parameterTypeList: ParameterList,
        public readonly returnParameterTypeList: ParameterList,
        public readonly visibility: Visibility,
        public readonly stateMutability: StateMutability) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitFunctionTypeName(this)) {
            this.parameterTypeList.accept(visitor);
            this.returnParameterTypeList.accept(visitor);
        }
        visitor.endVisitFunctionTypeName(this);
    }

    public isPayable(): boolean {
        return this.stateMutability === StateMutability.Payable;
    }
}

/**
 * Name referring to a user-defined type (i.e. a struct, contract, etc.).
 */
export class UserDefinedTypeName extends TypeName {
    constructor(
        location: SourceLocation,
        public readonly namePath: ReadonlyArray<string>
    ) {
        super(location);
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitUserDefinedTypeName(this);
        visitor.endVisitUserDefinedTypeName(this);
    }
}
