import { BigNumber as BN } from "bignumber.js";

import {
    Debug,
    MultiMap,
    applyMixins,
    arrayIsEqualTo,
    contains,
    createMap,
    createMultiMap,
    first,
    firstOrUndefined,
    isString,
    lastOrUndefined
} from "../core";
import {
    ElementaryTypeNameToken,
    TokenName,
    fromIdentifierOrKeyword,
    isAssignmentOp,
    isBinaryOp,
    isBitOp,
    isBooleanOp,
    isCompareOp,
    isElementaryTypeName,
    isShiftOp,
    isUnaryOp
} from "../parsing/token";
import {
    CancellationToken,
    CompilerOptions,
    Diagnostic,
    Path,
    ReadonlyMap,
    SourceLocation,
    Map as SymbolMap,
    bigint,
    rational,
    u256
} from "../types";
import { ASTVisitor } from "./astVisitor";
import { ExperimentalFeature } from "./experimentalFeatures";

const util = require("ethereumjs-util");

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
    public static resetID() {
        idDispenser.reset();
    }

    public static listAccept<T extends ASTNode>(list: ReadonlyArray<T>, visitor: ASTVisitor) {
        for (const element of list)
            element.accept(visitor);
    }

    private _id: number;

    private _location: SourceLocation;

    /// Annotation - is specialised in derived classes, is created upon request (because of polymorphism).
    protected _annotation: ASTAnnotation;

    public get id() {
        return this._id;
    }

    public get location(): SourceLocation {
        return this._location;
    }

    public get annotation(): ASTAnnotation {
        return this._annotation;
    }

    constructor(location: SourceLocation) {
        this._id = idDispenser.next();
        this._location = location;
    }

    public abstract accept(visitor: ASTVisitor): void;
}

export class SourceUnit extends ASTNode {
    constructor(
        location: SourceLocation,
        public readonly text: string,
        public readonly nodes: ReadonlyArray<ASTNode>
    ) {
        super(location);
    }

    public get annotation(): SourceUnitAnnotation {
        if (!this._annotation)
            this._annotation = new SourceUnitAnnotation();
        return this._annotation as SourceUnitAnnotation;
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitSourceUnit(this))
            ASTNode.listAccept(this.nodes, visitor);
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

    public get annotation(): ExpressionAnnotation {
        if (!this._annotation)
            this._annotation = new ExpressionAnnotation();
        return this._annotation as ExpressionAnnotation;
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

    public get annotation(): MemberAccessAnnotation {
        if (!this._annotation)
            this._annotation = new MemberAccessAnnotation();
        return this._annotation as MemberAccessAnnotation;
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

    public get annotation(): FunctionCallAnnotation {
        if (!this._annotation)
            this._annotation = new FunctionCallAnnotation();
        return this._annotation as FunctionCallAnnotation;
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitFunctionCall(this)) {
            this.expression.accept(visitor);
            ASTNode.listAccept(this.args, visitor);
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

    public get annotation(): BinaryOperationAnnotation {
        if (!this._annotation)
            this._annotation = new BinaryOperationAnnotation();
        return this._annotation as BinaryOperationAnnotation;
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

    public get annotation(): IdentifierAnnotation {
        if (!this._annotation)
            this._annotation = new IdentifierAnnotation();
        return this._annotation as IdentifierAnnotation;
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

    public get sourceUnitName(): string {
        return this.sourceUnit.annotation.path;
    }

    public isLValue(): boolean {
        return false;
    }
    public isPartOfExternalInterface(): boolean {
        return false;
    }

    /// @returns the type of expressions referencing this declaration.
    /// The current contract has to be given since this context can change the type, especially of
    /// contract types.
    /// This can only be called once types of variable declarations have already been resolved.
    public abstract get type(): Type;

    /// @param _internal false indicates external interface is concerned, true indicates internal interface is concerned.
    /// @returns null when it is not accessible as a function.
    public functionType(_internal: boolean): FunctionType | undefined {
        return undefined;
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

    public get annotation(): FunctionDefinitionAnnotation {
        if (!this._annotation)
            this._annotation = new FunctionDefinitionAnnotation();
        return this._annotation as FunctionDefinitionAnnotation;
    }

    public get type(): Type { return FunctionType.newFromFunctionDefinition(this); }

    public functionType(internal: boolean): FunctionType | undefined {
        if (internal) {
            switch (this.visibility) {
                case Visibility.Default:
                    Debug.assert(false, "visibility() should not return Default");
                case Visibility.Private:
                case Visibility.Internal:
                case Visibility.Public:
                    return FunctionType.newFromFunctionDefinition(this, internal);
                case Visibility.External:
                    return undefined;
                default:
                    Debug.assert(false, "visibility() should not return a Visibility");
            }
        }
        else {
            switch (this.visibility) {
                case Visibility.Default:
                    Debug.assert(false, "visibility() should not return Default");
                case Visibility.Private:
                case Visibility.Internal:
                    return undefined;
                case Visibility.Public:
                case Visibility.External:
                    return FunctionType.newFromFunctionDefinition(this, internal);
                default:
                    Debug.assert(false, "visibility() should not return a Visibility");
            }
        }
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitFunctionDefinition(this)) {
            this.parameterList.accept(visitor);
            if (this.returnParameterList)
                this.returnParameterList.accept(visitor);
            ASTNode.listAccept(this.modifiers, visitor);
            if (this.body)
                this.body.accept(visitor);
        }
        visitor.endVisitFunctionDefinition(this);
    }

    public isConstructor(): boolean { return this._isConstructor; }
    public isFallback(): boolean { return this.name === ""; }
    public isPayable(): boolean { return this.stateMutability === StateMutability.Payable; }
    public get returnParameters(): ReadonlyArray<VariableDeclaration> { return this.returnParameterList.parameters; }

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

    public get annotation(): VariableDeclarationAnnotation {
        if (!this._annotation)
            this._annotation = new VariableDeclarationAnnotation();
        return this._annotation as VariableDeclarationAnnotation;
    }

    public get type(): Type { return this.annotation.type; }

    public functionType(internal: boolean): FunctionType | undefined {
        if (internal)
            return undefined;
        switch (this.visibility) {
            case Visibility.Default:
                Debug.assert(false, "visibility() should not return Default");
            case Visibility.Private:
            case Visibility.Internal:
                return undefined;
            case Visibility.Public:
            case Visibility.External:
                return FunctionType.newFromVariableDeclaration(this);
            default:
                Debug.assert(false, "visibility() should not return a Visibility");
        }
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
            ASTNode.listAccept(this.args, visitor);
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
            ASTNode.listAccept(this.parameters, visitor);
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
            ASTNode.listAccept(this.args, visitor);
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

    public get annotation(): StatementAnnotation {
        if (!this._annotation)
            this._annotation = new StatementAnnotation();
        return this._annotation as StatementAnnotation;
    }
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
            ASTNode.listAccept(this.statements, visitor);
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

    public get annotation(): ReturnAnnotation {
        if (!this._annotation)
            this._annotation = new ReturnAnnotation();
        return this._annotation as ReturnAnnotation;
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

    public get annotation(): VariableDeclarationStatementAnnotation {
        if (!this._annotation)
            this._annotation = new VariableDeclarationStatementAnnotation();
        return this._annotation as VariableDeclarationStatementAnnotation;
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

    private _interfaceFunctionList: [string, FunctionType][];

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

    public get annotation(): ContractDefinitionAnnotation {
        if (!this._annotation)
            this._annotation = new ContractDefinitionAnnotation();
        return this._annotation as ContractDefinitionAnnotation;
    }

    public get type(): Type { return new TypeType(new ContractType(this)); }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitContractDefinition(this)) {
            ASTNode.listAccept(this.baseContracts, visitor);
            ASTNode.listAccept(this.subNodes, visitor);
        }
        visitor.endVisitContractDefinition(this);
    }

    public get usingForDirectives(): ReadonlyArray<UsingForDirective> {
        return filteredNodes(UsingForDirective, this.subNodes);
    }
    public get definedStructs(): ReadonlyArray<StructDefinition> {
        return filteredNodes(StructDefinition, this.subNodes);
    }
    public get definedEnums(): ReadonlyArray<EnumDefinition> {
        return filteredNodes(EnumDefinition, this.subNodes);
    }
    public get stateVariables(): ReadonlyArray<VariableDeclaration> {
        return filteredNodes(VariableDeclaration, this.subNodes);
    }
    public get functionModifiers(): ReadonlyArray<ModifierDefinition> {
        return filteredNodes(ModifierDefinition, this.subNodes);
    }
    public get definedFunctions(): ReadonlyArray<FunctionDefinition> {
        return filteredNodes(FunctionDefinition, this.subNodes);
    }
    public get events(): ReadonlyArray<EventDefinition> {
        return filteredNodes(EventDefinition, this.subNodes);
    }

    public isLibrary(): boolean {
        return this.contractKind === ContractKind.Library;
    }

    public get interfaceFunctions(): SymbolMap<FunctionType> {
        const exportedFunctionList = this.interfaceFunctionList;

        const exportedFunctions: SymbolMap<FunctionType> = createMap();
        for (const [k, v] of exportedFunctionList)
            exportedFunctions.set(k, v);

        Debug.assert(
            exportedFunctionList.length === exportedFunctions.size,
            "Hash collision at Function Definition Hash calculation");

        return exportedFunctions;
    }

    public get interfaceFunctionList(): [string, FunctionType][] {
        if (!this._interfaceFunctionList) {
            const signaturesSeen: Set<string> = new Set();
            this._interfaceFunctionList = [];
            for (const contract of this.annotation.linearizedBaseContracts) {
                const functions: FunctionType[] = [];
                for (const f of contract.definedFunctions) {
                    if (f.isPartOfExternalInterface())
                        functions.push(FunctionType.newFromFunctionDefinition(f, false));
                }
                for (const v of contract.stateVariables) {
                    if (v.isPartOfExternalInterface())
                        functions.push(FunctionType.newFromVariableDeclaration(v));
                }
                for (const fun of functions) {
                    if (!fun.interfaceFunctionType)
                        // Fails hopefully because we already registered the error
                        continue;
                    const functionSignature = fun.externalSignature;
                    if (!signaturesSeen.has(functionSignature)) {
                        signaturesSeen.add(functionSignature);
                        const hash = util.sha3(functionSignature).toString("hex");
                        this._interfaceFunctionList.push([hash, fun]);
                    }
                }
            }
        }
        return this._interfaceFunctionList;
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

            for (const f of this.definedFunctions)
                addInheritableMember(f);

            for (const v of this.stateVariables)
                addInheritableMember(v);

            for (const s of this.definedStructs)
                addInheritableMember(s);

            for (const e of this.definedEnums)
                addInheritableMember(e);

            for (const e of this.events)
                addInheritableMember(e);
        }
        return this._inheritableMembers;
    }

    /// Returns the constructor or undefined if no constructor was specified.
    public get constructorFunction(): FunctionDefinition | undefined {
        for (const f of this.definedFunctions) {
            if (f.isConstructor())
                return f;
        }
    }

    /// @returns true iff the constructor of this contract is public (or non-existing).
    public constructorIsPublic(): boolean {
        const f = this.constructorFunction;
        return !f || f.isPublic();
    }

    /// Returns the fallback function or nullptr if no fallback function was specified.
    public get fallbackFunction(): FunctionDefinition | undefined {
        for (const contract of this.annotation.linearizedBaseContracts) {
            for (const f of contract.definedFunctions) {
                if (f.isFallback())
                    return f;
            }
        }
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

    public get type(): Type {
        Debug.assert(!!this.annotation.sourceUnit);
        return new ModuleType(this.annotation.sourceUnit);
    }

    public get annotation(): ImportAnnotation {
        if (!this._annotation)
            this._annotation = new ImportAnnotation();
        return this._annotation as ImportAnnotation;
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

    public get annotation(): EventDefinitionAnnotation {
        if (!this._annotation)
            this._annotation = new EventDefinitionAnnotation();
        return this._annotation as EventDefinitionAnnotation;
    }

    public get type(): Type { return FunctionType.newFromEventDefinition(this); }

    public functionType(internal: boolean): FunctionType | undefined {
        if (internal)
            return FunctionType.newFromEventDefinition(this);
        else
            return undefined;
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitEventDefinition(this))
            this.parameterList.accept(visitor);
        visitor.endVisitEventDefinition(this);
    }

    public isAnonymous(): boolean { return this.anonymous; }
}
applyMixins(EventDefinition, [Documented]);

/**
 * Pseudo AST node that is used as declaration for "this", "msg", "tx", "block" and the global
 * functions when such an identifier is encountered. Will never have a valid location in the source code.
 */
export class MagicVariableDeclaration extends Declaration {
    constructor(name: string, public readonly type: Type) {
        super(new SourceLocation(), name);
    }
    public accept(_visitor: ASTVisitor) {
        Debug.assert(false, "MagicVariableDeclaration used inside real AST.");
    }
}

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

    public get annotation(): ModifierDefinitionAnnotation {
        if (!this._annotation)
            this._annotation = new ModifierDefinitionAnnotation();
        return this._annotation as ModifierDefinitionAnnotation;
    }

    public get type(): Type { return new ModifierType(this); }

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

    public get annotation(): TypeDeclarationAnnotation {
        if (!this._annotation)
            this._annotation = new TypeDeclarationAnnotation();
        return this._annotation as TypeDeclarationAnnotation;
    }

    public get type(): Type { return new TypeType(new StructType(this)); }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitStructDefinition(this)) {
            ASTNode.listAccept(this.members, visitor);
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

    public get type(): Type {
        return new TypeType(new EnumType(this));
    }

    public get annotation(): TypeDeclarationAnnotation {
        if (!this._annotation)
            this._annotation = new TypeDeclarationAnnotation();
        return this._annotation as TypeDeclarationAnnotation;
    }

    public accept(visitor: ASTVisitor) {
        if (visitor.visitEnumDefinition(this)) {
            ASTNode.listAccept(this.members, visitor);
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

    public get type(): Type {
        const parentDef = this.scope as EnumDefinition;
        Debug.assert(!!parentDef, "Enclosing Scope of EnumValue was not set");
        return new EnumType(parentDef);
    }
}

/**
 * Abstract base class of a type name, can be any built-in or user-defined type.
 */
export abstract class TypeName extends ASTNode {
    constructor(location: SourceLocation) {
        super(location);
    }

    public get annotation(): TypeNameAnnotation {
        if (!this._annotation)
            this._annotation = new TypeNameAnnotation();
        return this._annotation as TypeNameAnnotation;
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

    public get parameterTypes(): ReadonlyArray<VariableDeclaration> { return this.parameterTypeList.parameters; }
    public get returnParameterTypes(): ReadonlyArray<VariableDeclaration> { return this.returnParameterTypeList.parameters; }
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

    public get annotation(): UserDefinedTypeNameAnnotation {
        if (!this._annotation)
            this._annotation = new UserDefinedTypeNameAnnotation();
        return this._annotation as UserDefinedTypeNameAnnotation;
    }

    public accept(visitor: ASTVisitor) {
        visitor.visitUserDefinedTypeName(this);
        visitor.endVisitUserDefinedTypeName(this);
    }
}

export interface ScriptReferenceHost {
    getCompilerOptions(): CompilerOptions;
    getSourceFile(fileName: string): SourceUnit;
    getSourceFileByPath(path: Path): SourceUnit;
    getCurrentDirectory(): string;
}

export interface Program extends ScriptReferenceHost {
    /**
     * Get a list of root file names that were passed to a 'createProgram'
     */
    getRootFileNames(): ReadonlyArray<string>;

    /**
     * Get a list of files in the program
     */
    getSourceFiles(): ReadonlyArray<SourceUnit>;

    /**
     * Get a list of file names that were passed to 'createProgram' or referenced in a
     * program source file but could not be located.
     */
    /* @internal */
    getMissingFilePaths(): ReadonlyArray<Path>;

    getSyntacticDiagnostics(sourceFile?: SourceUnit, cancellationToken?: CancellationToken): ReadonlyArray<Diagnostic>;
}

export class ASTAnnotation {
}

export interface DocTag {
    content: string;	///< The text content of the tag.
    paramName: string;	///< Only used for @param, stores the parameter name.
}

export class DocumentedAnnotation {
    /// Mapping docstring tag name -> content.
    public docTags: MultiMap<DocTag> = createMultiMap<DocTag>();
}

export class SourceUnitAnnotation extends ASTAnnotation {
    /// The "absolute" (in the compiler sense) path of this source unit.
    public path = "";
    /// The exported symbols (all global symbols).
    public exportedSymbols: ReadonlyMap<Declaration[]> = new Map();
    /// Experimental features.
    public experimentalFeatures: Set<ExperimentalFeature> = new Set();
}

export class ImportAnnotation extends ASTAnnotation {
    /// The absolute path of the source unit to import.
    public absolutePath = "";
    /// The actual source unit.
    public sourceUnit: SourceUnit;
}

export class StatementAnnotation extends ASTAnnotation implements DocumentedAnnotation {
    public docTags: MultiMap<DocTag>;
}
applyMixins(StatementAnnotation, [DocumentedAnnotation]);

export class ReturnAnnotation extends StatementAnnotation {
    /// Reference to the return parameters of the function.
    public functionReturnParameters: ParameterList;
}

export class VariableDeclarationStatementAnnotation extends StatementAnnotation {
    /// Information about which component of the value is assigned to which variable.
    /// The pointer can be null to signify that the component is discarded.
    public assignments: VariableDeclaration[] = [];
}

export class ExpressionAnnotation extends ASTAnnotation {
    /// Inferred type of the expression.
    public type: Type;
    /// Whether the expression is a constant variable
    public isConstant = false;
    /// Whether the expression is pure, i.e. compile-time constant.
    public isPure = false;
    /// Whether it is an LValue (i.e. something that can be assigned to).
    public isLValue = false;
    /// Whether the expression is used in a context where the LValue is actually required.
    public lValueRequested = false;
    /// Types of arguments if the expression is a function that is called - used
    /// for overload resolution
    public argumentTypes: Type[] = [];
}

export class BinaryOperationAnnotation extends ExpressionAnnotation {
    /// The common type that is used for the operation, not necessarily the result type (which
    /// e.g. for comparisons is bool).
    public commonType: Type;
};

export class IdentifierAnnotation extends ExpressionAnnotation {
    /// Referenced declaration, set at latest during overload resolution stage.
    public referencedDeclaration: Declaration;
    /// List of possible declarations it could refer to.
    public overloadedDeclarations: Declaration[] = [];
}

export class MemberAccessAnnotation extends ExpressionAnnotation {
    /// Referenced declaration, set at latest during overload resolution stage.
    public referencedDeclaration: Declaration;
}

export const enum FunctionCallKind {
    Unset,
    FunctionCall,
    TypeConversion,
    StructConstructorCall
}

export class FunctionCallAnnotation extends ExpressionAnnotation {
    public kind: FunctionCallKind = FunctionCallKind.Unset;
}

export class ModifierDefinitionAnnotation extends ASTAnnotation implements DocumentedAnnotation {
    public docTags: MultiMap<DocTag>;
}
applyMixins(ModifierDefinitionAnnotation, [DocumentedAnnotation]);

export class EventDefinitionAnnotation extends ASTAnnotation implements DocumentedAnnotation {
    public docTags: MultiMap<DocTag>;
}
applyMixins(EventDefinitionAnnotation, [DocumentedAnnotation]);

export class FunctionDefinitionAnnotation extends ASTAnnotation implements DocumentedAnnotation {
    public docTags: MultiMap<DocTag>;

    /// The function this function overrides, if any. This is always the closest
    /// in the linearized inheritance hierarchy.
    public superFunction: FunctionDefinition;
}
applyMixins(FunctionDefinitionAnnotation, [DocumentedAnnotation]);

export class TypeDeclarationAnnotation extends ASTAnnotation {
    /// The name of this type, prefixed by proper namespaces if globally accessible.
    public canonicalName = "";
}

export class VariableDeclarationAnnotation extends ASTAnnotation {
    /// Type of variable (type of identifier referencing this variable).
    public type: Type;
}

export class TypeNameAnnotation extends ASTAnnotation {
    /// Type declared by this type name, i.e. type of a variable where this type name is used.
    /// Set during reference resolution stage.
    public type: Type;
}

export class UserDefinedTypeNameAnnotation extends TypeNameAnnotation {
    /// Referenced declaration, set during reference resolution stage.
    public referencedDeclaration: Declaration;

    /// Stores a reference to the current contract.
    /// This is needed because types of base contracts change depending on the context.
    public contractScope: ContractDefinition;
}

export class ContractDefinitionAnnotation extends TypeDeclarationAnnotation implements DocumentedAnnotation {
    public docTags: MultiMap<DocTag>;

    /// List of functions without a body. Can also contain functions from base classes.
    public unimplementedFunctions: FunctionDefinition[];
    /// List of all (direct and indirect) base contracts in order from derived to
    /// base, including the contract itself.
    public linearizedBaseContracts: ContractDefinition[];
    /// List of contracts this contract creates, i.e. which need to be compiled first.
    /// Also includes all contracts from @a linearizedBaseContracts.
    public contractDependencies: ContractDefinition[];
}
applyMixins(StatementAnnotation, [DocumentedAnnotation]);

export const enum TypeCategory {
    Integer,
    RationalNumber,
    StringLiteral,
    Bool,
    FixedPoint,
    Array,
    FixedBytes,
    Contract,
    Struct,
    Function,
    Enum,
    Tuple,
    Mapping,
    TypeType,
    Modifier,
    Magic,
    Module,
    InaccessibleDynamic
}

export const enum DataLocation { Storage, CallData, Memory }

/**
 * Helper class to compute storage offsets of members of structs and contracts.
 */
class StorageOffsets {
    private _storageSize: u256;
    private _offsets: Map<number, [u256, number]>;

    /// Resets the StorageOffsets objects and determines the position in storage for each
    /// of the elements of @a _types.
    public computeOffsets(types: Type[]) {
        let slotOffset: bigint = new BN(0);
        let byteOffset = 0;
        const offsets = new Map<number, [u256, number]>();
        for (let i = 0; i < types.length; ++i) {
            const type = types[i];
            if (!type.canBeStored())
                continue;
            if (byteOffset + type.storageBytes > 32) {
                // would overflow, go to next slot
                slotOffset = slotOffset.add(1);
                byteOffset = 0;
            }
            if (slotOffset.greaterThanOrEqualTo(new BN(2).pow(256)))
                throw new Error("Object too large for storage.");
            offsets.set(i, [slotOffset, byteOffset]);
            Debug.assert(type.storageSize.greaterThanOrEqualTo(1), "Invalid storage size.");
            if (type.storageSize.equals(1) && byteOffset + type.storageBytes <= 32)
                byteOffset += type.storageBytes;
            else {
                slotOffset = slotOffset.add(type.storageSize);
                byteOffset = 0;
            }
        }
        if (byteOffset > 0)
            slotOffset = slotOffset.add(1);
        if (slotOffset.greaterThanOrEqualTo(new BN(2).pow(256)))
            throw new Error("Object too large for storage.");
        this._storageSize = slotOffset;
        this._offsets = offsets;
    }

    /// @returns the offset of the given member, might be null if the member is not part of storage.
    public offset(index: number): [u256, number] | undefined {
        if (this._offsets.size > 0) {
            return this._offsets.get(index);
        }
        else {
            return undefined;
        }
    }

    /// @returns the total number of slots occupied by all members.
    public get storageSize(): u256 { return this._storageSize; }
};

export class Member {
    constructor(public readonly name: string,
        public readonly type: Type,
        public readonly declaration?: Declaration) {
    }
}

export type MemberMap = Member[];

export class MemberList {
    private _storageOffsets: StorageOffsets;

    constructor(public readonly memberTypes: MemberMap) {
    }

    public combine(other: MemberList) {
        this.memberTypes.push(...other.memberTypes);
    }

    public memberType(name: string): Type {
        let type: Type;
        for (const memberType of this.memberTypes) {
            if (memberType.name === name) {
                Debug.assert(!type, "Requested member type by non-unique name.");
                type = memberType.type;
            }
        }
        return type;
    }

    public membersByName(name: string): MemberMap {
        const members: MemberMap = [];
        for (const memberType of this.memberTypes) {
            if (memberType.name === name)
                members.push(memberType);
        }
        return members;
    }

    /// @returns the offset of the given member in storage slots and bytes inside a slot or
    /// a nullptr if the member is not part of storage.
    public memberStorageOffset(name: string): [u256, number] | undefined {
        if (!this._storageOffsets) {
            const memberTypes: Type[] = [];
            for (const member of this.memberTypes)
                memberTypes.push(member.type);
            this._storageOffsets = new StorageOffsets();
            this._storageOffsets.computeOffsets(memberTypes);
        }
        for (let index = 0; index < this.memberTypes.length; ++index) {
            if (this.memberTypes[index].name === name)
                return this._storageOffsets.offset(index);
        }
        return undefined;
    }

    /// @returns the number of storage slots occupied by the members.
    public get storageSize(): u256 {
        // trigger lazy computation
        this.memberStorageOffset("");
        return this._storageOffsets.storageSize;
    }
}

export abstract class Type {
    /// List of member types (parameterised by scape), will be lazy-initialized.
    protected _members: Map<ContractDefinition, MemberList>;

    public static forLiteral(literal: Literal) {
        switch (literal.token) {
            case TokenName.TrueLiteral:
            case TokenName.FalseLiteral:
                return new BoolType();
            case TokenName.Number:
                {
                    const validLiteral = RationalNumberType.isValidLiteral(literal);
                    if (validLiteral)
                        return new RationalNumberType(validLiteral);
                    else
                        return undefined;
                }
            case TokenName.StringLiteral:
                return new StringLiteralType(literal);
            default:
                return undefined;
        }
    }

    public static fromElementaryTypeName(name: string | ElementaryTypeNameToken): Type {
        if (isString(name)) {
            const characterCodes = stringToCharCodes(name);
            const { token, m, n } = fromIdentifierOrKeyword(characterCodes);
            return Type.fromElementaryTypeName(new ElementaryTypeNameToken(token, m, n));
        }

        const type = name as ElementaryTypeNameToken;
        Debug.assert(isElementaryTypeName(type.token),
            "Expected an elementary type name but got " + type.toString()
        );

        const token = type.token;
        const m = type.firstNumber;
        const n = type.secondNumber;

        switch (token) {
            case TokenName.IntM:
                return new IntegerType(m, IntegerTypeModifier.Signed);
            case TokenName.UIntM:
                return new IntegerType(m, IntegerTypeModifier.Unsigned);
            case TokenName.BytesM:
                return new FixedBytesType(m);
            case TokenName.FixedMxN:
                return new FixedPointType(m, n, FixedPointTypeModifier.Signed);
            case TokenName.UFixedMxN:
                return new FixedPointType(m, n, FixedPointTypeModifier.Unsigned);
            case TokenName.Int:
                return new IntegerType(256, IntegerTypeModifier.Signed);
            case TokenName.UInt:
                return new IntegerType(256, IntegerTypeModifier.Unsigned);
            case TokenName.Fixed:
                return new FixedPointType(128, 19, FixedPointTypeModifier.Signed);
            case TokenName.UFixed:
                return new FixedPointType(128, 19, FixedPointTypeModifier.Unsigned);
            case TokenName.Byte:
                return new FixedBytesType(1);
            case TokenName.Address:
                return new IntegerType(160, IntegerTypeModifier.Address);
            case TokenName.Bool:
                return new BoolType();
            case TokenName.Bytes:
                return ArrayType.newBytesOrString(DataLocation.Storage);
            case TokenName.String:
                return ArrayType.newBytesOrString(DataLocation.Storage, true);
            //no types found
            default:
                Debug.assert(
                    false,
                    "Unable to convert elementary typename " + type.toString() + " to type."
                );
        }
    }

    public abstract get category(): TypeCategory;

    /// @returns a valid solidity identifier such that two types should compare equal if and
    /// only if they have the same identifier.
    /// The identifier should start with "t_".
    /// More complex identifier strings use "parentheses", where $_ is interpreted as as
    /// "opening parenthesis", _$ as "closing parenthesis", _$_ as "comma" and any $ that
    /// appears as part of a user-supplied identifier is escaped as _$$$_.
    public abstract get identifier(): string;

    public isImplicitlyConvertibleTo(other: Type): boolean { return this === other; }
    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        return this.isImplicitlyConvertibleTo(convertTo);
    }

    /// @returns the resulting type of applying the given unary operator or an empty pointer if
    /// this is not possible.
    /// The default implementation does not allow any unary operator.
    public unaryOperatorResult(_operator: TokenName): Type | undefined { return undefined; }

    /// @returns the resulting type of applying the given binary operator or an empty pointer if
    /// this is not possible.
    /// The default implementation allows comparison operators if a common type exists
    public binaryOperatorResult(operator: TokenName, other: Type): Type | undefined {
        return isCompareOp(operator) ? commonType(this, other) : undefined;
    }

    public equals(other: Type): boolean { return this.category === other.category; }

    /// @returns number of bytes used by this type when encoded for CALL. If it is a dynamic type,
    /// returns the size of the pointer (usually 32). Returns 0 if the type cannot be encoded
    /// in calldata.
    /// @note: This should actually not be called on types, where isDynamicallyEncoded returns true.
    /// If @a _padded then it is assumed that each element is padded to a multiple of 32 bytes.
    public calldataEncodedSize(_padded = true): number { return 0; }

    /// @returns the size of this data type in bytes when stored in memory. For memory-reference
    /// types, this is the size of the memory pointer.
    public get memoryHeadSize(): number { return this.calldataEncodedSize(); }

    /// @returns true if the type is a dynamic array
    public isDynamicallySized(): boolean { return false; }

    /// @returns true if the type is dynamically encoded in the ABI
    public isDynamicallyEncoded(): boolean { return false; }

    /// @returns the number of storage slots required to hold this value in storage.
    /// For dynamically "allocated" types, it returns the size of the statically allocated head,
    public get storageSize(): u256 { return new BN(1); }

    /// Multiple small types can be packed into a single storage slot. If such a packing is possible
    /// this function @returns the size in bytes smaller than 32. Data is moved to the next slot if
    /// it does not fit.
    /// In order to avoid computation at runtime of whether such moving is necessary, structs and
    /// array data (not each element) always start a new slot.
    public get storageBytes(): number { return 32; }

    /// Returns true if the type can be stored in storage.
    public canBeStored(): boolean { return true; }

    /// Returns false if the type cannot live outside the storage, i.e. if it includes some mapping.
    public canLiveOutsideStorage(): boolean { return true; }
    /// Returns true if the type can be stored as a value (as opposed to a reference) on the stack,
    /// i.e. it behaves differently in lvalue context and in value context.
    public isValueType(): boolean { return false; }
    public get sizeOnStack(): number { return 1; }

    /// @returns the mobile (in contrast to static) type corresponding to the given type.
    /// This returns the corresponding IntegerType or FixedPointType for RationalNumberType
    /// and the pointer type for storage reference types.
    /// Might return a null pointer if there is no fitting type.
    public get mobileType(): Type { return this; }

    /// @returns true if this is a non-value type and the data of this type is stored at the
    /// given location.
    public dataStoredIn(_location: DataLocation): boolean { return false; }

    /// Returns the list of all members of this type. Default implementation: no members apart from bound.
    /// @param _currentScope scope in which the members are accessed.
    public members(currentScope: ContractDefinition): MemberList {
        if (!this._members.get(currentScope)) {
            const members = this.nativeMembers(currentScope);
            if (currentScope)
                members.push(...boundFunctions(this, currentScope));
            this._members.set(currentScope, new MemberList(members.slice()));
        }
        return this._members.get(currentScope);
    }

    /// @returns the members native to this type depending on the given context. This function
    /// is used (in conjunction with boundFunctions to fill m_members below.
    protected nativeMembers(_currentScope: ContractDefinition): MemberMap {
        return [];
    }

    /// Convenience method, returns the type of the given named member or an empty pointer if no such member exists.
    public memberType(name: string, currentScope?: ContractDefinition): Type {
        return this.members(currentScope).memberType(name);
    }

    /// @returns the type of a temporary during assignment to a variable of the given type.
    /// Specifically, returns the requested itself if it can be dynamically allocated (or is a value type)
    /// and the mobile type otherwise.
    public closestTemporaryType(targetType: Type): Type {
        return targetType.dataStoredIn(DataLocation.Storage) ? this.mobileType : targetType;
    }

    public abstract toString(short: boolean): string;

    /// @returns the canonical name of this type for use in library function signatures.
    public get canonicalName(): string { return this.toString(true); }

    /// @returns the signature of this type in external functions, i.e. `uint256` for integers
    /// or `(uint256,bytes8)[2]` for an array of structs. If @a _structsByName,
    /// structs are given by canonical name like `ContractName.StructName[2]`.
    public signatureInExternalFunction(_structsByName: boolean): string {
        return this.canonicalName;
    }

    public literalValue(_contract?: Literal): u256 {
        Debug.assert(false, "Literal value requested for type without literals.");
        return new BN(0);
    }

    /// @returns a (simpler) type that is encoded in the same way for external function calls.
    /// This for example returns address for contract types.
    /// If there is no such type, returns an empty shared pointer.
    public get encodingType(): Type | undefined { return undefined; }

    /// @returns a (simpler) type that is used when decoding this type in calldata.
    public get decodingType(): Type | undefined { return this.encodingType; }

    /// @returns a type that will be used outside of Solidity for e.g. function signatures.
    /// This for example returns address for contract types.
    /// If there is no such type, returns an empty shared pointer.
    /// @param _inLibrary if set, returns types as used in a library, e.g. struct and contract types
    /// are returned without modification.
    public interfaceType(_inLibrary: boolean): Type | undefined { return undefined; }

    /// should be have identical to !!interfaceType(inLibrary) but might do optimizations.
    public canBeUsedExternally(inLibrary: boolean): boolean {
        return !!this.interfaceType(inLibrary);
    }
}

/// @returns a member list containing all members added to this type by `using for` directives.
function boundFunctions(_type: Type, scope: ContractDefinition): MemberMap {
    // Normalise data location of type.
    const type = ReferenceType.copyForLocationIfReference(DataLocation.Storage, _type);
    const seenFunctions = new Set<Declaration>();
    const members: MemberMap = [];
    for (const contract of scope.annotation.linearizedBaseContracts) {
        for (const ufd of contract.usingForDirectives) {
            if (ufd.typeName &&
                !type.equals(ReferenceType.copyForLocationIfReference(DataLocation.Storage, ufd.typeName.annotation.type)))
                continue;

            const library = ufd.libraryName.annotation.referencedDeclaration as ContractDefinition;
            for (const funDecl of library.definedFunctions) {
                if (!funDecl.isVisibleInDerivedContracts() || seenFunctions.has(funDecl))
                    continue;
                seenFunctions.add(funDecl);
                const funType = FunctionType.newFromFunctionDefinition(funDecl, false);
                const fun = funType.asMemberFunction(true, true);
                if (fun) {
                    if (type.isImplicitlyConvertibleTo(fun.selfType))
                        members.push(new Member(funDecl.name, fun, funDecl));
                }
            }
        }
    }
    return members;
}

export function commonType(a: Type, b: Type): Type | undefined {
    if (!a || !b)
        return undefined;
    else if (a.mobileType && b.isImplicitlyConvertibleTo(a.mobileType))
        return a.mobileType;
    else if (b.mobileType && a.isImplicitlyConvertibleTo(b.mobileType))
        return b.mobileType;
    else
        return undefined;
}

export const enum IntegerTypeModifier {
    Unsigned, Signed, Address
}

/**
 * Any kind of integer type (signed, unsigned, address).
 */
export class IntegerType extends Type {
    public get category(): TypeCategory { return TypeCategory.Integer; }

    constructor(
        private readonly bits: number,
        private readonly modifier = IntegerTypeModifier.Unsigned) {
        super();
        if (this.isAddress())
            Debug.assert(bits === 160);
        Debug.assert(
            bits > 0 && bits <= 256 && bits % 8 === 0,
            "Invalid bit number for integer type: " + bits
        );
    }

    public get identifier(): string {
        if (this.isAddress())
            return "t_address";
        else
            return "t_" + (this.isSigned() ? "" : "u") + "int" + this.numBits;
    }

    public equals(other: Type): boolean {
        if (other.category !== this.category)
            return false;
        return other instanceof IntegerType &&
            other.bits === this.bits &&
            other.modifier === this.modifier;
    }

    public isImplicitlyConvertibleTo(convertTo: Type): boolean {
        if (convertTo instanceof IntegerType) {
            if (convertTo.bits < this.bits)
                return false;
            if (this.isAddress())
                return convertTo.isAddress();
            else if (this.isSigned())
                return convertTo.isSigned();
            else
                return !convertTo.isSigned() || convertTo.bits > this.bits;
        }
        else if (convertTo instanceof FixedPointType) {
            if (this.isAddress())
                return false;
            else
                return this.maxValue.lessThanOrEqualTo(convertTo.maxIntegerValue) && this.minValue.greaterThanOrEqualTo(convertTo.minIntegerValue);
        }
        else
            return false;
    }

    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        return convertTo.category === this.category ||
            convertTo.category === TypeCategory.Contract ||
            convertTo.category === TypeCategory.Enum ||
            convertTo.category === TypeCategory.FixedBytes ||
            convertTo.category === TypeCategory.FixedPoint;
    }

    public unaryOperatorResult(operator: TokenName): Type | undefined {
        // "delete" is ok for all integer types
        if (operator === TokenName.Delete)
            return new TupleType();
        // no further unary operators for addresses
        else if (this.isAddress())
            return undefined;
        // for non-address integers, we allow +, -, ++ and --
        else if (operator === TokenName.Add || operator === TokenName.Sub ||
            operator === TokenName.Inc || operator === TokenName.Dec ||
            operator === TokenName.BitNot)
            return this;
        else
            return undefined;
    }

    public binaryOperatorResult(operator: TokenName, other: Type): Type | undefined {
        if (other.category !== TypeCategory.RationalNumber &&
            other.category !== TypeCategory.FixedPoint &&
            other.category !== this.category)
            return undefined;

        if (isShiftOp(operator)) {
            // Shifts are not symmetric with respect to the type
            if (this.isAddress())
                return undefined;
            if (isValidShiftAndAmountType(operator, other))
                return this;
            else
                return undefined;
        }

        const _commonType = commonType(this, other); //might be a integer or fixed point
        if (!_commonType)
            return undefined;

        // All integer types can be compared
        if (isCompareOp(operator))
            return _commonType;
        if (isBooleanOp(operator))
            return undefined;
        if (_commonType instanceof IntegerType) {
            // Nothing else can be done with addresses
            if (_commonType.isAddress())
                return undefined;
            // Signed EXP is not allowed
            if (TokenName.Exp === operator && _commonType.isSigned())
                return undefined;
        }
        else if (_commonType instanceof FixedPointType) {
            if (TokenName.Exp === operator)
                return undefined;
        }
        return _commonType;
    }

    public calldataEncodedSize(padded = true): number { return padded ? 32 : Math.floor(this.bits / 8); }
    public get storageBytes(): number { return Math.floor(this.bits / 8); }
    public isValueType(): boolean { return true; }

    public nativeMembers(_const: ContractDefinition): MemberMap {
        if (this.isAddress())
            return [
                new Member("balance", new IntegerType(256)),
                new Member("call", new FunctionType(
                    [],
                    ["bool"],
                    [],
                    [],
                    FunctionKind.BareCall,
                    true,
                    StateMutability.Payable)),
                new Member("callcode", new FunctionType(
                    [],
                    ["bool"],
                    [],
                    [],
                    FunctionKind.BareCallCode,
                    true,
                    StateMutability.Payable)),
                new Member("delegatecall", new FunctionType(
                    [],
                    ["bool"],
                    [],
                    [],
                    FunctionKind.BareDelegateCall,
                    true)),
                new Member("send", new FunctionType(
                    ["uint"],
                    ["bool"],
                    [],
                    [],
                    FunctionKind.Send)),
                new Member("transfer", new FunctionType(
                    ["uint"],
                    [],
                    [],
                    [],
                    FunctionKind.Transfer))
            ];
        else
            return [];
    }

    public toString(_short: boolean): string {
        if (this.isAddress())
            return "address";
        const prefix = this.isSigned() ? "int" : "uint";
        return prefix + this.bits;
    }

    public literalValue(literal?: Literal): u256 {
        Debug.assert(this.modifier === IntegerTypeModifier.Address);
        Debug.assert(!!literal);
        Debug.assert(literal.value.substr(0, 2) === "0x");
        return new BN(literal.value);
    }

    public get encodingType(): Type { return this; }
    public interfaceType(_inLibrary: boolean): Type | undefined { return this; }

    public get numBits(): number { return this.bits; }
    public isAddress(): boolean { return this.modifier === IntegerTypeModifier.Address; }
    public isSigned(): boolean { return this.modifier === IntegerTypeModifier.Signed; }

    public get minValue(): bigint {
        if (this.isSigned())
            return new BN(2).pow(this.bits - 1).neg();
        else
            return new BN(0);
    }
    public get maxValue(): bigint {
        if (this.isSigned())
            return new BN(2).pow(this.bits - 1).sub(1);
        else
            return new BN(2).pow(this.bits).sub(1);
    }
}

export const enum FixedPointTypeModifier {
    Unsigned, Signed
}

/**
 * A fixed point type number (signed, unsigned).
 */
export class FixedPointType extends Type {
    public get category(): TypeCategory { return TypeCategory.FixedPoint; }

    constructor(public readonly numBits: number,
        public readonly fractionalDigits: number,
        private modifier = FixedPointTypeModifier.Unsigned) {
        super();
        Debug.assert(
            8 <= numBits && numBits <= 256 && numBits % 8 === 0 &&
            0 <= fractionalDigits && fractionalDigits <= 80,
            "Invalid bit number(s) for fixed type: " +
            numBits + "x" + fractionalDigits
        );
    }

    public get identifier(): string {
        return "t_" + (this.isSigned() ? "" : "u") + "fixed" + this.numBits + "x" + this.fractionalDigits;
    }

    public equals(other: Type): boolean {
        if (other.category !== this.category)
            return false;
        return other instanceof FixedPointType &&
            other.numBits === this.numBits &&
            other.fractionalDigits === this.fractionalDigits &&
            other.modifier === this.modifier;
    }

    public isImplicitlyConvertibleTo(convertTo: Type): boolean {
        if (convertTo instanceof FixedPointType) {
            if (convertTo.numBits < this.numBits || convertTo.fractionalDigits < this.fractionalDigits)
                return false;
            else
                return convertTo.maxIntegerValue.greaterThanOrEqualTo(this.maxIntegerValue) && convertTo.minIntegerValue.lessThanOrEqualTo(this.minIntegerValue);
        }
        return false;
    }

    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        return convertTo.category === this.category ||
            convertTo.category === TypeCategory.Integer ||
            convertTo.category === TypeCategory.FixedBytes;
    }

    public unaryOperatorResult(operator: TokenName): Type | undefined {
        switch (operator) {
            case TokenName.Delete:
                // "delete" is ok for all fixed types
                return new TupleType();
            case TokenName.Add:
            case TokenName.Sub:
            case TokenName.Inc:
            case TokenName.Dec:
                // for fixed, we allow +, -, ++ and --
                return this;
            default:
                return undefined;
        }
    }

    public binaryOperatorResult(operator: TokenName, other: Type): Type | undefined {
        if (
            other.category !== TypeCategory.RationalNumber &&
            other.category !== this.category &&
            other.category !== TypeCategory.Integer)
            return undefined;

        const _commonType = commonType(this, other); //might be fixed point or integer
        if (!_commonType)
            return undefined;

        // All fixed types can be compared
        if (isCompareOp(operator))
            return _commonType;
        if (isBitOp(operator) || isBooleanOp(operator))
            return undefined;

        if (_commonType instanceof FixedPointType) {
            if (TokenName.Exp === operator)
                return undefined;
        }
        else if (_commonType instanceof IntegerType) {
            if (_commonType.isAddress())
                return undefined;
        }
        return _commonType;
    }

    public calldataEncodedSize(padded = true): number { return padded ? 32 : Math.floor(this.numBits / 8); }
    public get storageBytes(): number { return Math.floor(this.numBits / 8); }
    public isValueType(): boolean { return true; }

    public toString(_short: boolean): string {
        const prefix = this.isSigned() ? "fixed" : "ufixed";
        return prefix + this.numBits + "x" + this.fractionalDigits;
    }

    public get encodingType(): Type { return this; }
    public interfaceType(_inLibrary: boolean): Type | undefined { return this; }

    public isSigned(): boolean { return this.modifier === FixedPointTypeModifier.Signed; }

    /// @returns the largest integer value this type con hold. Note that this is not the
    /// largest value in general.
    public get maxIntegerValue(): bigint {
        const maxValue = new BN(2).pow(this.numBits - (this.isSigned() ? 1 : 0)).sub(1);
        return maxValue.div(new BN(10).pow(this.fractionalDigits)).floor();
    }

    /// @returns the smallest integer value this type can hold. Note hat this is not the
    /// smallest value in general.
    public get minIntegerValue(): bigint {
        if (this.isSigned()) {
            const minValue = new BN(2).pow(this.numBits - (this.isSigned() ? 1 : 0)).neg();
            return minValue.div(new BN(10, this.fractionalDigits)).floor();
        }
        else
            return new BN(0);
    }
}

/**
 * Integer and fixed point constants either literals or computed.
 * Example expressions: 2, 3.14, 2+10.2, ~10.
 * There is one distinct type per value.
 */
export class RationalNumberType extends Type {
    public get category(): TypeCategory { return TypeCategory.RationalNumber; }

    /// @returns true if the literal is a valid integer.
    public static isValidLiteral(_literal: Literal): rational | undefined {
        throw new Error("Not implemented");
    }

    constructor(public readonly value: rational) {
        super();
    }

    public isImplicitlyConvertibleTo(_convertTo: Type): boolean {
        throw new Error("Not implemented");
    }
    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        const mobType = this.mobileType;
        return mobType && mobType.isExplicitlyConvertibleTo(convertTo);
    }

    public unaryOperatorResult(operator: TokenName): Type | undefined {
        let value = new BN(0);
        switch (operator) {
            case TokenName.BitNot:
                if (this.isFractional())
                    return undefined;
                const [numerator,] = this.value.toFraction();
                value = new BN(~(new BN(numerator).toNumber()));
                break;
            case TokenName.Add:
                value = value.add(this.value);
                break;
            case TokenName.Sub:
                value = value.sub(this.value);
                break;
            case TokenName.After:
                return this;
            default:
                return undefined;
        }
        return new RationalNumberType(value);
    }
    public binaryOperatorResult(_operator: TokenName, _other: Type): Type | undefined {
        throw new Error("Not implemented");
    }

    public get identifier(): string {
        const [numerator, denominator] = this.value.toFraction();
        return "t_rational_" + numerator + "_by_" + denominator;
    }

    public equals(other: Type): boolean {
        return other instanceof RationalNumberType && this.value.equals(other.value);
    }

    public canBeStored(): boolean { return false; }
    public canLiveOutsideStorage(): boolean { return false; }

    public toString(_short: boolean): string {
        const [numerator, denominator] = this.value.toFraction();
        if (!this.isFractional()) {
            return "int_const " + numerator;
        }
        return "rational_const " + numerator + '/' + denominator;
    }

    public literalValue(_literal?: Literal): u256 {
        throw new Error("Not implemented");
    }

    public get mobileType(): Type {
        if (!this.isFractional())
            return this.integerType;
        else
            return this.fixedPointType;
    }

    /// @returns the smallest integer type that can hold the value or an empty pointer if not possible.
    public get integerType(): IntegerType | undefined {
        Debug.assert(!this.isFractional(), "integerType() called for fractional number.");
        const [numerator,] = this.value.toFraction();
        let value = new BN(numerator);
        const negative = value.isNeg();
        if (negative) // convert to positive number of same bit requirements
            value = value.neg().sub(1).mul(2); // ((0 - value) - 1) << 1;
        if (value.greaterThan(-1))
            return undefined;
        else
            return new IntegerType(
                Math.max(bytesRequired(value), 1) * 8,
                negative ? IntegerTypeModifier.Signed : IntegerTypeModifier.Unsigned
            );
    }

    /// @returns the smallest fixed type that can  hold the value or incurs the least precision loss.
    /// If the integer part does not fit, returns an empty pointer.
    public get fixedPointType(): FixedPointType | undefined {
        const negative = this.value.isNeg();
        let fractionalDigits = 0;
        let value = this.value.abs(); // We care about the sign later.
        const maxValue = negative ?
            new BN(2).pow(255) :
            new BN(2).pow(256).sub(1);

        let [, denominator] = value.toFraction();
        while (value.mul(10).lessThanOrEqualTo(maxValue) && denominator !== "1" && fractionalDigits < 80) {
            value = value.mul(10);
            fractionalDigits++;
            denominator = value.toFraction()[1];
        }

        if (value.greaterThan(maxValue))
            return undefined;
        // This means we round towards zero for positive and negative values.
        let [_numerator, _denominator] = value.toFraction();
        let v = new BN(_numerator).div(new BN(_denominator)).floor();
        if (negative)
            // modify value to satisfy bit requirements for negative numbers:
            // add one bit for sign and decrement because negative numbers can be larger
            v = v.sub(1).mul(2);

        if (v.greaterThan(-1))
            return undefined;

        const totalBits = Math.max(bytesRequired(v), 1) * 8;
        Debug.assert(totalBits <= 256);

        return new FixedPointType(
            totalBits, fractionalDigits,
            negative ? FixedPointTypeModifier.Signed : FixedPointTypeModifier.Unsigned
        );
    }

    /// @returns true if the value is not an integer.
    public isFractional(): boolean {
        const [, denominator] = this.value.toFraction();
        return denominator !== "1";
    }

    /// @returns true if the value is negative.
    public isNegative(): boolean { return this.value.isNegative(); }
}

/**
 * Literal string, can be converted to bytes, bytesX or string.
 */
export class StringLiteralType extends Type {
    private readonly value: string;

    public get category(): TypeCategory { return TypeCategory.StringLiteral; }

    constructor(literal: Literal) {
        super();
        this.value = literal.value;
    }

    public isImplicitlyConvertibleTo(_convertTo: Type): boolean {
        if (_convertTo instanceof FixedBytesType)
            return _convertTo.numBytes >= this.value.length;
        else if (_convertTo instanceof ArrayType)
            return _convertTo.isByteArray() &&
                !(_convertTo.dataStoredIn(DataLocation.Storage) && _convertTo.isPointer()) &&
                !(_convertTo.isString() && !this.isValidUTF8());
        else
            return false;
    }

    public binaryOperatorResult(_operator: TokenName, _other: Type): Type | undefined { return undefined; }

    public get identifier(): string {
        // Since we have to return a valid identifier and the string itself may contain
        // anything, we hash it.
        return "t_stringliteral_" + util.sha3(this.value).toString("hex");
    }

    public equals(other: Type): boolean {
        return other instanceof StringLiteralType &&
            this.value === other.value;
    }

    public canBeStored(): boolean { return false; }
    public canLiveOutsideStorage(): boolean { return false; }
    public get sizeOnStack(): number { return 0; }

    public toString(_short: boolean): string {
        // FIXME: Return an error if this.value is not a valid UTF8 string.
        return "literal_string \"" + this.value + "\"";
    }

    public get mobileType(): Type {
        return ArrayType.newBytesOrString(DataLocation.Memory, true);
    }

    public isValidUTF8(): boolean {
        // FIXME: Check if this.value is a valid UTF8 string.
        return true;
    }
}

/**
 * Bytes type with fixed length of up to 32 bytes.
 */
export class FixedBytesType extends Type {
    public get category(): TypeCategory { return TypeCategory.FixedBytes; }

    /// @returns the smallest bytes type for the given literal or an empty pointer
    /// if no type fits.
    public static smallestTypeForLiteral(literal: string): FixedBytesType | undefined {
        if (literal.length <= 32)
            return new FixedBytesType(literal.length);
        return undefined;
    }

    constructor(public readonly numBytes: number) {
        super();
        Debug.assert(numBytes >= 0 && numBytes <= 32,
            "Invalid byte number for fixed bytes type: " + numBytes);
    }

    public isImplicitlyConvertibleTo(convertTo: Type): boolean {
        if (convertTo instanceof FixedBytesType)
            return convertTo.numBytes >= this.numBytes;
        else
            return false;
    }
    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        return convertTo.category === TypeCategory.Integer ||
            convertTo.category === TypeCategory.FixedPoint ||
            convertTo.category === TypeCategory.Contract ||
            convertTo.category === this.category;
    }

    public get identifier(): string { return "t_bytes" + this.numBytes; }

    public equals(other: Type): boolean {
        return other instanceof FixedBytesType &&
            other.numBytes === this.numBytes;
    }

    public unaryOperatorResult(_operator: TokenName): Type | undefined {
        // "delete" and "~" is okay for FixedBytesType
        if (_operator === TokenName.Delete)
            return new TupleType();
        else if (_operator === TokenName.BitNot)
            return this;
        return undefined;
    }

    public binaryOperatorResult(operator: TokenName, other: Type): Type | undefined {
        if (isShiftOp(operator)) {
            if (isValidShiftAndAmountType(operator, other))
                return this;
            else
                return undefined;
        }

        const _commonType = commonType(this, other) as FixedBytesType;
        if (!_commonType)
            return undefined;

        // FixedBytes can be compared and have bitwise operators applied to them
        if (isCompareOp(operator) || isBitOp(operator))
            return _commonType;

        return undefined;
    }

    public calldataEncodedSize(padded: boolean): number {
        return padded && this.numBytes > 0 ? 32 : this.numBytes;
    }

    public get storageBytes(): number { return this.numBytes; }
    public isValueType(): boolean { return true; }

    public toString(_short: boolean): string { return "bytes" + this.numBytes; }

    public nativeMembers(_contract: ContractDefinition): MemberMap {
        return [new Member("length", new IntegerType(8))];
    }

    public get encodingType(): Type { return this; }
    public interfaceType(_inLibrary: boolean): Type | undefined { return this; }
}

/**
 * The boolean type.
 */
export class BoolType extends Type {
    public get category(): TypeCategory { return TypeCategory.Bool; }
    public get identifier(): string { return "t_bool"; }

    public unaryOperatorResult(operator: TokenName): Type | undefined {
        if (operator === TokenName.Delete)
            return new TupleType();
        return (operator === TokenName.Not) ? this : undefined;

    }
    public binaryOperatorResult(operator: TokenName, other: Type): Type | undefined {
        if (!(other instanceof BoolType))
            return undefined;

        if (isCompareOp(operator) || operator === TokenName.And || operator === TokenName.Or)
            return other;
        else
            return undefined;
    }

    public calldataEncodedSize(padded: boolean): number { return padded ? 32 : 1; }
    public get storageBytes(): number { return 1; }
    public isValueType(): boolean { return true; }

    public toString(_short: boolean): string { return "bool"; }
    public literalValue(literal?: Literal): u256 {
        Debug.assert(!!literal);
        if (literal.token === TokenName.TrueLiteral)
            return new BN(1);
        else if (literal.token === TokenName.FalseLiteral)
            return new BN(0);
        else
            Debug.assert(false, "Bool type constructed from non-boolean literal.");
    }

    public get encodingType(): Type { return this; }
    public interfaceType(_: boolean): Type | undefined { return this; }
};


/**
 * Base class used by types which are not value types and can be stored either in storage, memory
 * or calldata. This is currently used by arrays and structs.
 */
export abstract class ReferenceType extends Type {
    protected _isPointer = true;

    constructor(public readonly location: DataLocation) {
        super();
    }

    public unaryOperatorResult(operator: TokenName): Type | undefined {
        if (operator !== TokenName.Delete)
            return undefined;
        // delete can be used on everything except calldata references or storage pointers
        // (storage references are ok)
        switch (this.location) {
            case DataLocation.CallData:
                return undefined;
            case DataLocation.Memory:
                return new TupleType();
            case DataLocation.Storage:
                return this._isPointer ? undefined : new TupleType();
            default:
                Debug.assert(false);
        }
        return undefined;
    }
    public binaryOperatorResult(_operator: TokenName, _other: Type): Type | undefined { return undefined; }

    public get memoryHeadSize(): number { return 32; }

    /// @returns a copy of this type with location (recursively) changed to @a _location,
    /// whereas isPointer is only shallowly changed - the deep copy is always a bound reference.
    public abstract copyForLocation(_location: DataLocation, _isPointer: boolean): Type;

    public get mobileType(): Type { return this.copyForLocation(this.location, true); }

    public dataStoredIn(location: DataLocation): boolean { return this.location === location; }

    /// Storage references can be pointers or bound references. In general, local variables are of
    /// pointer type, state variables are bound references. Assignments to pointers or deleting
    /// them will not modify storage (that will only change the pointer). Assignment from
    /// non-storage objects to a variable of storage pointer type is not possible.
    public isPointer(): boolean { return this._isPointer; }

    public equals(other: Type): boolean {
        return other instanceof ReferenceType &&
            this.location === other.location &&
            this._isPointer === other._isPointer;
    }

    /// @returns a copy of @a _type having the same location as this (and is not a pointer type)
    /// if _type is a reference type and an unmodified copy of _type otherwise.
    /// This function is mostly useful to modify inner types appropriately.
    public static copyForLocationIfReference(location: DataLocation, type: Type): Type {
        if (type instanceof ReferenceType) {
            return type.copyForLocation(location, false);
        }
        return type;
    }

    protected copyForLocationIfReference(type: Type): Type {
        return ReferenceType.copyForLocationIfReference(this.location, type);
    }

    /// @returns a human-readable description of the reference part of the type.
    protected get stringForReferencePart(): string {
        switch (this.location) {
            case DataLocation.Storage:
                return "storage " + (this._isPointer ? "pointer" : "ref");
            case DataLocation.CallData:
                return "calldata";
            case DataLocation.Memory:
                return "memory";
        }
        Debug.assert(false);
        return "";
    }

    /// @returns the suffix computed from the reference part to be used by identifier();
    protected get identifierLocationSuffix(): string {
        let id = "";
        if (this.location === DataLocation.Storage)
            id += "_storage";
        else if (this.location === DataLocation.Memory)
            id += "_memory";
        else
            id += "_calldata";
        if (this._isPointer)
            id += "_ptr";
        return id;
    }
}

/// String is interpreted as a subtype of Bytes.
export const enum ArrayKind { Ordinary, Bytes, String }

/**
 * The type of an array. The flavours are byte array (bytes), statically- (<type>[<length>])
 * and dynamically-sized array (<type>[]).
 * In storage, all arrays are packed tightly (as long as more than one elementary type fits in
 * one slot). Dynamically sized arrays (including byte arrays) start with their size as a uint and
 * thus start on their own slot.
 */
export class ArrayType extends ReferenceType {
    ///< Byte arrays ("bytes") and strings have different semantics from ordinary arrays.

    public get category(): TypeCategory { return TypeCategory.Array; }

    /// Constructor for a byte array ("bytes") and string.
    public static newBytesOrString(location: DataLocation, isString = false): ArrayType {
        return new ArrayType(location,
            new FixedBytesType(1),
            isString ? ArrayKind.String : ArrayKind.Bytes);
    }

    /// Constructor for a dynamically sized array type ("type[]")
    public static newDynamicSizeArray(location: DataLocation, baseType: Type): ArrayType {
        return new ArrayType(location,
            baseType);
    }

    /// Constructor for a fixed-size array type ("type[20]")
    public static newFixedSizeArray(location: DataLocation, baseType: Type, length: u256): ArrayType {
        const hasDynamicLength = false;
        return new ArrayType(location,
            baseType,
            ArrayKind.Ordinary,
            hasDynamicLength,
            length);
    }

    private constructor(
        location: DataLocation,
        public baseType: Type,
        private arrayKind = ArrayKind.Ordinary,
        private hasDynamicLength = true,
        public length: u256 = new BN(0)
    ) {
        super(location);
        this.baseType = this.copyForLocationIfReference(baseType);
    }

    public isImplicitlyConvertibleTo(convertTo: Type): boolean {
        if (!(convertTo instanceof ArrayType))
            return false;
        if (convertTo.isByteArray() !== this.isByteArray() || convertTo.isString() !== this.isString())
            return false;
        // memory/calldata to storage can be converted, but only to a direct storage reference
        if (convertTo.location === DataLocation.Storage && this.location !== DataLocation.Storage && convertTo.isPointer())
            return false;
        if (convertTo.location === DataLocation.CallData && this.location !== convertTo.location)
            return false;
        if (convertTo.location === DataLocation.Storage && !convertTo.isPointer()) {
            // Less restrictive conversion, since we need to copy anyway.
            if (!this.baseType.isImplicitlyConvertibleTo(convertTo.baseType))
                return false;
            if (convertTo.isDynamicallySized())
                return true;
            return !this.isDynamicallySized() && convertTo.length.greaterThanOrEqualTo(this.length);
        }
        else {
            // Conversion to storage pointer or to memory, we de not copy element-for-element here, so
            // require that the base type is the same, not only convertible.
            // This disallows assignment of nested dynamic arrays from storage to memory for now.
            if (ReferenceType.copyForLocationIfReference(this.location, this.baseType) !=
                ReferenceType.copyForLocationIfReference(this.location, convertTo.baseType))
                return false;
            if (this.isDynamicallySized() !== convertTo.isDynamicallySized())
                return false;
            // We also require that the size is the same.
            if (!this.isDynamicallySized() && !this.length.equals(convertTo.length))
                return false;
            return true;
        }
    }
    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        if (this.isImplicitlyConvertibleTo(convertTo))
            return true;
        // allow conversion bytes <-> string
        if (!(convertTo instanceof ArrayType))
            return false;
        if (convertTo.location !== this.location)
            return false;
        if (!this.isByteArray() || !convertTo.isByteArray())
            return false;
        return true;
    }

    public get identifier(): string {
        let id;
        if (this.isString())
            id = "t_string";
        else if (this.isByteArray())
            id = "t_bytes";
        else {
            id = "t_array";
            id += identifierList(this.baseType);
            if (this.isDynamicallySized())
                id += "dyn";
            else
                id += this.length.toFixed();
        }
        id += this.identifierLocationSuffix;

        return id;
    }

    public equals(other: Type): boolean {
        if (!(other instanceof ArrayType))
            return false;
        if (
            !super.equals(other) ||
            other.isByteArray() !== this.isByteArray() ||
            other.isString() !== this.isString() ||
            other.isDynamicallySized() !== this.isDynamicallySized()
        )
            return false;
        if (!other.baseType.equals(this.baseType))
            return false;
        return this.isDynamicallySized() || this.length.equals(other.length);
    }

    public calldataEncodedSize(padded: boolean): number {
        const size = this.unlimitedCalldataEncodedSize(padded);
        Debug.assert(size.lessThanOrEqualTo(new BN(2, 32).sub(1)), "Array size does not fit unsigned.");
        return size.toNumber();
    }
    public isDynamicallySized(): boolean { return this.hasDynamicLength; }
    public isDynamicallyEncoded(): boolean {
        return this.isDynamicallySized() || this.baseType.isDynamicallyEncoded();
    }
    public get storageSize(): u256 {
        if (this.isDynamicallySized())
            return new BN(1);

        let size: u256;
        const baseBytes = this.baseType.storageBytes;
        if (baseBytes === 0)
            size = new BN(1);
        else if (baseBytes < 32) {
            const itemsPerSlot = Math.floor(32 / baseBytes);
            size = new BN(this.length).add(itemsPerSlot - 1).div(itemsPerSlot).floor();
        }
        else
            size = new BN(this.length).mul(this.baseType.storageSize);
        if (size.greaterThanOrEqualTo(new BN(2).pow(256)))
            throw new Error("Array too large for storage.");
        return BN.max(1, size);
    }
    public canLiveOutsideStorage(): boolean { return this.baseType.canLiveOutsideStorage(); }
    public get sizeOnStack(): number {
        if (this.location === DataLocation.CallData)
            // offset [length] (stack top)
            return 1 + (this.isDynamicallySized() ? 1 : 0);
        else
            // storage slot or memory offset
            // byte offset inside storage value is omitted
            return 1;
    }

    public toString(short: boolean): string {
        let ret: string;
        if (this.isString())
            ret = "string";
        else if (this.isByteArray())
            ret = "bytes";
        else {
            ret = this.baseType.toString(short) + "[";
            if (!this.isDynamicallySized())
                ret += this.length.toFixed();
            ret += "]";
        }
        if (!short)
            ret += " " + this.stringForReferencePart;
        return ret;
    }

    public get canonicalName(): string {
        let ret;
        if (this.isString())
            ret = "string";
        else if (this.isByteArray())
            ret = "bytes";
        else {
            ret = this.baseType.canonicalName + "[";
            if (!this.isDynamicallySized())
                ret += length;
            ret += "]";
        }
        return ret;
    }
    public signatureInExternalFunction(_structsByName: boolean): string {
        if (this.isByteArray())
            return this.canonicalName;
        else {
            Debug.assert(!!this.baseType);
            return this.baseType.signatureInExternalFunction(_structsByName) +
                "[" +
                (this.isDynamicallySized() ? "" : this.length.toFixed()) +
                "]";
        }
    }

    public nativeMembers(_currentScope: ContractDefinition): MemberMap {
        const members: MemberMap = [];
        if (!this.isString()) {
            members.push(new Member("length", new IntegerType(256)));
            if (this.isDynamicallySized() && this.location === DataLocation.Storage)
                members.push(new Member(
                    "push", new FunctionType(
                        [this.baseType],
                        [new IntegerType(256)],
                        [],
                        [],
                        this.isByteArray() ? FunctionKind.ByteArrayPush : FunctionKind.ArrayPush)));
        }
        return members;
    }

    public get encodingType(): Type {
        if (this.location === DataLocation.Storage)
            return new IntegerType(256);
        else
            return this.copyForLocation(DataLocation.Memory, true);
    }
    public get decodingType(): Type {
        if (this.location === DataLocation.Storage)
            return new IntegerType(256);
        else
            return this;
    }
    public interfaceType(_inLibrary: boolean): Type | undefined {
        // Note: This has to fulfill canBeUsedExternally(_inLibrary) === !!interfaceType(_inLibrary)
        if (_inLibrary && this.location === DataLocation.Storage)
            return this;

        if (this.arrayKind !== ArrayKind.Ordinary)
            return this.copyForLocation(DataLocation.Memory, true);
        const baseExt = this.baseType.interfaceType(_inLibrary);
        if (!baseExt)
            return undefined;

        if (this.isDynamicallySized())
            return new ArrayType(DataLocation.Memory, baseExt);
        else
            return ArrayType.newFixedSizeArray(DataLocation.Memory, baseExt, this.length);
    }
    public canBeUsedExternally(_inLibrary: boolean): boolean {
        // Note: This has to fulfill canBeUsedExternally(_inLibrary) ===  !!interfaceType(_inLibrary)
        if (_inLibrary && this.location === DataLocation.Storage)
            return true;
        else if (this.arrayKind !== ArrayKind.Ordinary)
            return true;
        else if (!this.baseType.canBeUsedExternally(_inLibrary))
            return false;
        else if (this.baseType.category === TypeCategory.Array && this.baseType.isDynamicallySized())
            return false;
        else
            return true;
    }

    /// @returns true if this is valid to be stored in calldata
    public get validForCalldata(): boolean {
        return this.unlimitedCalldataEncodedSize(true).lessThanOrEqualTo(new BN(2, 32).sub(1));
    }

    /// @returns true if this is a byte array or a string
    public isByteArray(): boolean { return this.arrayKind !== ArrayKind.Ordinary; }

    /// @returns true if this is a string
    public isString(): boolean { return this.arrayKind === ArrayKind.String; }

    public get memorySize(): u256 {
        Debug.assert(!this.isDynamicallySized());
        Debug.assert(this.location === DataLocation.Memory);
        const size = new BN(this.length).mul(this.baseType.memoryHeadSize);
        Debug.assert(size.lessThanOrEqualTo(new BN(2, 32).sub(1)), "Array size does not fit u256.");
        return size;
    }

    public copyForLocation(location: DataLocation, isPointer: boolean): Type {
        const copy = ArrayType.newBytesOrString(location);
        copy._isPointer = isPointer;
        copy.arrayKind = this.arrayKind;
        copy.baseType = copy.copyForLocationIfReference(this.baseType);
        copy.hasDynamicLength = this.hasDynamicLength;
        copy.length = this.length;
        return copy;
    }

    private unlimitedCalldataEncodedSize(_padded: boolean): bigint {
        if (this.isDynamicallySized())
            return new BN(32);
        let size = new BN(this.length).mul(this.isByteArray() ? 1 : this.baseType.calldataEncodedSize(_padded));
        size = size.add(31).sub(32).floor().mul(32); // ((size + 31) / 32) * 32;
        return size;
    }
}

/**
 * The type of a contract instance or library, there is one distinct type for each contract definition.
 */
export class ContractType extends Type {
    /// Type of the constructor, @see constructorType. Lazily initialized.
    private _constructorType: FunctionType;

    public get category(): TypeCategory { return TypeCategory.Contract; }

    constructor(public readonly contractDefinition: ContractDefinition,
        private readonly _isSuper = false) {
        super();
    }

    /// Contracts can be implicitly converted to super classes and to addresses.
    public isImplicitlyConvertibleTo(convertTo: Type): boolean {
        if (this.equals(convertTo))
            return true;
        if (convertTo instanceof IntegerType)
            return convertTo.isAddress();
        if (convertTo instanceof ContractType) {
            const bases = this.contractDefinition.annotation.linearizedBaseContracts;
            if (this._isSuper && bases.length <= 1)
                return false;
            return contains(this._isSuper ? bases.slice(1) : bases, convertTo.contractDefinition);
        }
        return false;
    }
    /// Contracts can be converted to themselves and to integers.
    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        return this.isImplicitlyConvertibleTo(convertTo) ||
            convertTo.category === TypeCategory.Integer ||
            convertTo.category === TypeCategory.Contract;
    }

    public unaryOperatorResult(operator: TokenName): Type | undefined {
        return operator === TokenName.Delete ? new TupleType() : undefined;
    }

    public get identifier(): string {
        return (this._isSuper ? "t_super" : "t_contract") + parenthesizeUserIdentifier(this.contractDefinition.name) + this.contractDefinition.id;
    }

    public equals(other: Type): boolean {
        return other instanceof ContractType &&
            other.contractDefinition === this.contractDefinition &&
            other._isSuper === this._isSuper;
    }

    public calldataEncodedSize(padded: boolean): number {
        return this.encodingType.calldataEncodedSize(padded);
    }

    public get storageBytes(): number { return 20; }
    public canLiveOutsideStorage(): boolean { return true; }
    public get sizeOnStack(): number { return this._isSuper ? 0 : 1; }
    public isValueType(): boolean { return true; }

    public toString(_short: boolean): string {
        return (this.contractDefinition.isLibrary() ? "library " : "contract ") +
            (this._isSuper ? "super " : "") + this.contractDefinition.name;
    }
    public get canonicalName(): string {
        return this.contractDefinition.annotation.canonicalName;
    }

    public nativeMembers(contract: ContractDefinition): MemberMap {
        const members: MemberMap = [];
        Debug.assert(!!contract);
        if (this._isSuper) {
            // add the most derived of all functions which are visible in derived contracts
            const bases = this.contractDefinition.annotation.linearizedBaseContracts;
            Debug.assert(bases.length >= 1, "linearizedBaseContracts should at least contain the most derived contract.");
            // `sliced(1, ...)` ignores the most derived contract, which should not be searchable from `super`.
            for (const base of bases.slice(1)) {
                for (const funDef of base.definedFunctions) {
                    if (!funDef.isVisibleInDerivedContracts())
                        continue;
                    const functionType = FunctionType.newFromFunctionDefinition(funDef, true);
                    let functionWithEqualArgumentsFound = false;
                    for (const member of members) {
                        if (member.name !== funDef.name)
                            continue;
                        const memberType = member.type as FunctionType;
                        Debug.assert(!!memberType, "Override changes type.");
                        if (!memberType.hasEqualArgumentTypes(functionType))
                            continue;
                        functionWithEqualArgumentsFound = true;
                        break;
                    }
                    if (!functionWithEqualArgumentsFound)
                        members.push(new Member(
                            funDef.name,
                            functionType,
                            funDef
                        ));
                }
            }
        }
        else if (!this.contractDefinition.isLibrary()) {
            this.contractDefinition.interfaceFunctions.forEach(functionType => {
                members.push(new Member(
                    functionType.declaration.name,
                    functionType.asMemberFunction(this.contractDefinition.isLibrary()),
                    functionType.declaration));
            });
        }
        // In 0.5.0 address members are not populated into the contract.
        if (!this.contractDefinition.sourceUnit.annotation.experimentalFeatures.has(ExperimentalFeature.V050))
            ContractType.addNonConflictingAddressMembers(members);
        return members;
    }

    public get encodingType(): Type {
        return new IntegerType(160, IntegerTypeModifier.Address);
    }
    public interfaceType(_inLibrary: boolean): Type | undefined {
        return _inLibrary ? this : this.encodingType;
    }

    public isSuper(): boolean { return this._isSuper; }

    // @returns true if and only if the contract has a payable fallback function
    public isPayable(): boolean {
        const fallbackFunction = this.contractDefinition.fallbackFunction;
        return fallbackFunction && fallbackFunction.isPayable();
    }

    /// Returns the function type of the constructor modified to return an object of the contract's type.
    public newExpressionType(): FunctionType {
        if (!this._constructorType)
            this._constructorType = FunctionType.newExpressionType(this.contractDefinition);
        return this._constructorType;
    }

    /// @returns the identifier of the function with the given name or Invalid256 if such a name does
    /// not exist.
    /// @returns a list of all state variables (including inherited) of the contract and their
    /// offsets in storage.
    public get stateVariables(): [VariableDeclaration, u256, number][] {
        const variables: VariableDeclaration[] = [];
        for (const contract of this.contractDefinition.annotation.linearizedBaseContracts.reverse()) {
            for (const variable of contract.stateVariables)
                if (!variable.isConstant())
                    variables.push(variable);
        }
        const types: Type[] = [];
        for (const variable of variables)
            types.push(variable.annotation.type);
        const offsets = new StorageOffsets();
        offsets.computeOffsets(types);

        const variablesAndOffsets: [VariableDeclaration, u256, number][] = [];
        for (let index = 0; index < variables.length; ++index) {
            const offset = offsets.offset(index);
            if (offset)
                variablesAndOffsets.push([variables[index], offset[0], offset[1]]);
        }
        return variablesAndOffsets;
    }

    private static addNonConflictingAddressMembers(members: MemberMap) {
        const addressMembers = new IntegerType(160, IntegerTypeModifier.Address).nativeMembers(null);
        for (const addressMember of addressMembers) {
            let clash = false;
            for (const member of members) {
                if (member.name === addressMember.name &&
                    (
                        // Members with different types are not allowed
                        member.type.category !== addressMember.type.category ||
                        // Members must overload functions without clash
                        (
                            member.type.category === TypeCategory.Function &&
                            (member.type as FunctionType).hasEqualArgumentTypes(addressMember.type as FunctionType)
                        )
                    )
                ) {
                    clash = true;
                    break;
                }
            }

            if (!clash)
                members.push(new Member(
                    addressMember.name,
                    addressMember.type,
                    addressMember.declaration
                ));
        }
    }
}

/**
 * The type of a struct instance, there is one distinct type per struct definition.
 */
export class StructType extends ReferenceType {
    /// Cache for the recursive() function.
    private _recursive?: boolean;

    public get category(): TypeCategory { return TypeCategory.Struct; }

    constructor(public readonly structDefinition: StructDefinition, location = DataLocation.Storage) {
        super(location);
    }

    public isImplicitlyConvertibleTo(convertTo: Type): boolean {
        if (!(convertTo instanceof StructType))
            return false;
        // memory/calldata to storage can be converted, but only to a direct storage reference
        if (convertTo.location === DataLocation.Storage && this.location !== DataLocation.Storage && convertTo.isPointer())
            return false;
        if (convertTo.location === DataLocation.CallData && this.location !== convertTo.location)
            return false;
        return this.structDefinition === convertTo.structDefinition;
    }

    public get identifier(): string {
        return "t_struct" + parenthesizeUserIdentifier(this.structDefinition.name) + this.structDefinition.id + this.identifierLocationSuffix;
    }

    public equals(other: Type): boolean {
        return other instanceof StructType &&
            super.equals(other) &&
            other.structDefinition === this.structDefinition;
    }

    public calldataEncodedSize(padded: boolean): number {
        let size = 0;
        for (const member of this.members(null).memberTypes) {
            if (!member.type.canLiveOutsideStorage())
                return 0;
            else {
                let memberSize = member.type.calldataEncodedSize(padded);
                if (memberSize === 0)
                    return 0;
                size += memberSize;
            }
        }
        return size;
    }
    public isDynamicallyEncoded(): boolean {
        Debug.assert(!this._recursive);
        for (let t of this.memoryMemberTypes) {
            Debug.assert(!!t, "Parameter should have external type.");
            t = t.interfaceType(false);
            if (t.isDynamicallyEncoded())
                return true;
        }
        return false;
    }

    public get memorySize(): u256 {
        let size: u256 = new BN(0);
        for (const t of this.memoryMemberTypes)
            size = size.add(t.memoryHeadSize);
        return size;
    }

    public get storageSize(): u256 {
        return BN.max(1, this.members(null).storageSize);
    }
    public canLiveOutsideStorage(): boolean { return true; }

    public toString(short: boolean): string {
        let ret = "struct " + this.structDefinition.annotation.canonicalName;
        if (!short)
            ret += " " + this.stringForReferencePart;
        return ret;
    }

    public nativeMembers(_currentScope: ContractDefinition): MemberMap {
        const members: MemberMap = [];
        for (const variable of this.structDefinition.members) {
            const type = variable.annotation.type;
            Debug.assert(!!type);
            // Skip all mapping members if we are not in storage.
            if (this.location !== DataLocation.Storage && !type.canLiveOutsideStorage())
                continue;
            members.push(new Member(
                variable.name,
                this.copyForLocationIfReference(type),
                variable)
            );
        }
        return members;
    }

    public get encodingType(): Type {
        return this.location === DataLocation.Storage ? new IntegerType(256) : this;
    }
    public interfaceType(_inLibrary: boolean): Type | undefined {
        if (!this.canBeUsedExternally(_inLibrary))
            return undefined;

        // Has to fulfill canBeUsedExternally(_inLibrary) === !!interfaceType(_inLibrary)
        if (_inLibrary && this.location === DataLocation.Storage)
            return this;
        else
            return this.copyForLocation(DataLocation.Memory, true);
    }

    public canBeUsedExternally(inLibrary: boolean): boolean {
        if (inLibrary && this.location === DataLocation.Storage)
            return true;
        else if (this.recursive)
            return false;
        else {
            // Check that all members have interface types.
            // We pass "false" to canBeUsedExternally (_inLibrary), because this struct will be
            // passed by value and thus the encoding does not differ, but it will disallow
            // mappings.
            for (const varDecl of this.structDefinition.members) {
                if (!varDecl.annotation.type.canBeUsedExternally(false))
                    return false;
            }
        }
        return true;
    }

    public copyForLocation(location: DataLocation, isPointer: boolean): Type {
        const copy = new StructType(this.structDefinition, location);
        copy._isPointer = isPointer;
        return copy;
    }

    public get canonicalName(): string {
        return this.structDefinition.annotation.canonicalName;
    }

    public signatureInExternalFunction(_structsByName: boolean): string {
        if (_structsByName)
            return this.canonicalName;
        else {
            const memberTypes = this.memoryMemberTypes;
            const memberTypeStrings = memberTypes.map(_t => {
                Debug.assert(!!_t, "Parameter should have external type.");
                const t = _t.interfaceType(_structsByName);
                Debug.assert(!!t, "");
                return t.signatureInExternalFunction(_structsByName);
            });
            return "(" + memberTypeStrings.join(",") + ")";
        }
    }

    /// @returns a function that peforms the type conversion between a list of struct members
    /// and a memory struct of this type.
    public get constructorType(): FunctionType {
        const paramTypes: Type[] = [];
        const paramNames: string[] = [];
        for (const member of this.members(null).memberTypes) {
            if (!member.type.canLiveOutsideStorage())
                continue;
            paramNames.push(member.name);
            paramTypes.push(ReferenceType.copyForLocationIfReference(DataLocation.Memory, member.type));
        }
        return new FunctionType(
            paramTypes,
            [this.copyForLocation(DataLocation.Memory, false)],
            paramNames,
            [],
            FunctionKind.Internal,
            false,
            StateMutability.NonPayable);
    }

    public storageOffsetsOfMember(name: string): [u256, number] {
        const offsets = this.members(null).memberStorageOffset(name);
        Debug.assert(!!offsets, "Storage offset of non-existing member requested.");
        return offsets;
    }

    public memoryOffsetOfMember(_name: string): u256 {
        let offset: u256 = new BN(0);
        for (const member of this.members(null).memberTypes) {
            if (member.name === _name)
                return offset;
            else
                offset = offset.add(member.type.memoryHeadSize);
        }
        Debug.assert(false, "Member not found in struct.");
        return new BN(0);
    }

    /// @returns the vector of types of members available in memory.
    public get memoryMemberTypes(): Type[] {
        const types: Type[] = [];
        for (const variable of this.structDefinition.members) {
            if (variable.annotation.type.canLiveOutsideStorage())
                types.push(variable.annotation.type);
        }
        return types;
    }

    /// @returns the set of all members that are removed in the memory version (typically mappings).
    public get membersMissingInMemory(): Set<string> {
        const missing: Set<string> = new Set();
        for (const variable of this.structDefinition.members)
            if (!variable.annotation.type.canLiveOutsideStorage())
                missing.add(variable.name);
        return missing;
    }

    /// @returns true if the same struct is used recursively in one of its members. Only
    /// analyses the "memory" representation, i.e. mappings are ignored in all structs.
    public get recursive(): boolean {
        if (!this._recursive) {
            const structsSeen: Set<StructDefinition> = new Set();
            function check(t: StructType): boolean {
                const str = t.structDefinition;
                if (structsSeen.has(str))
                    return true;
                structsSeen.add(str);
                for (const variable of str.members) {
                    let memberType = variable.annotation.type;
                    while (memberType instanceof ArrayType) {
                        memberType = memberType.baseType;
                    }
                    if (memberType instanceof StructType) {
                        if (check(memberType))
                            return true;
                    }
                }
                return false;
            }
            this._recursive = check(this);
        }
        return this._recursive;
    }
}

/**
 * The type of an enum instance, there is one distinct type per enum definition.
 */
export class EnumType extends Type {
    public get category(): TypeCategory { return TypeCategory.Enum; }

    constructor(public readonly enumDefinition: EnumDefinition) {
        super();
    }

    public unaryOperatorResult(operator: TokenName): Type | undefined {
        return operator === TokenName.Delete ? new TupleType() : undefined;
    }

    public get identifier(): string {
        return "t_enum" + parenthesizeUserIdentifier(this.enumDefinition.name) + this.enumDefinition.id;
    }

    public equals(other: Type): boolean {
        if (other.category !== this.category)
            return false;
        return other instanceof EnumType && other.enumDefinition === this.enumDefinition;
    }

    public calldataEncodedSize(padded: boolean): number {
        return this.encodingType.calldataEncodedSize(padded);
    }
    public get storageBytes(): number {
        let elements = this.numberOfMembers;
        if (elements <= 1)
            return 1;
        else
            return bytesRequired(elements - 1);
    }
    public canLiveOutsideStorage(): boolean { return true; }

    public toString(_short: boolean): string {
        return "enum " + this.enumDefinition.annotation.canonicalName;
    }
    public get canonicalName(): string {
        return this.enumDefinition.annotation.canonicalName;
    }

    public isValueType(): boolean { return true; }

    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        return convertTo.equals(this) || convertTo.category === TypeCategory.Integer;
    }

    public get encodingType(): Type {
        return new IntegerType(8 * this.storageBytes);
    }
    public interfaceType(inLibrary: boolean): Type | undefined {
        return inLibrary ? this : this.encodingType;
    }

    /// @returns the value that the string has in the Enum
    public memberValue(member: string): number {
        let index = 0;
        for (const decl of this.enumDefinition.members) {
            if (decl.name === member)
                return index;
            ++index;
        }
        Debug.assert(false, "Requested unknown enum value " + member);
    }

    public get numberOfMembers(): number {
        return this.enumDefinition.members.length;
    }
}

/**
 * Type that can hold a finite sequence of values of different types.
 * In some cases, the components are empty pointers (when used as placeholders).
 */
export class TupleType extends Type {
    public get category(): TypeCategory { return TypeCategory.Tuple; }

    constructor(public readonly components: Type[] = []) {
        super();
    }

    public isImplicitlyConvertibleTo(other: Type): boolean {
        if (other instanceof TupleType) {
            const targets = other.components;
            if (targets.length === 0)
                return this.components.length === 0;
            if (this.components.length !== targets.length && !firstOrUndefined(targets) && !lastOrUndefined(targets))
                return false; // (,a,) = (1,2,3,4) - unable to position `a` in the tuple.
            let minNumValues = targets.length;
            if (!lastOrUndefined(targets) || !firstOrUndefined(targets))
                --minNumValues; // wildcards can also match 0 components
            if (this.components.length < minNumValues)
                return false;
            if (this.components.length > targets.length && firstOrUndefined(targets) && lastOrUndefined(targets))
                return false; // larger source and no wildcard
            const fillRight = !lastOrUndefined(targets) || firstOrUndefined(targets);
            for (let i = 0; i < Math.min(targets.length, this.components.length); ++i) {
                const s = this.components[fillRight ? i : this.components.length - i - 1];
                const t = targets[fillRight ? i : targets.length - i - 1];
                if (!s && t)
                    return false;
                else if (s && t && !s.isImplicitlyConvertibleTo(t))
                    return false;
            }
            return true;
        } else
            return false;
    }

    public get identifier(): string {
        return "t_tuple" + identifierList(...this.components);
    }

    public equals(other: Type): boolean {
        return other instanceof TupleType &&
            arrayIsEqualTo(this.components, other.components, (a, b) => a.equals(b));
    }

    public binaryOperatorResult(_operator: TokenName, _other: Type): Type | undefined { return undefined; }

    public toString(short: boolean): string {
        if (this.components.length === 0)
            return "tuple()";
        let str = "tuple(";
        str += this.components.map(t => t ? t.toString(short) : "").join(",");
        return str + ")";
    }

    public canBeStored(): boolean { return false; }

    public get storageSize(): u256 {
        Debug.assert(false, "Storage size of non-storable tuple type requested.");
        return new BN(0);
    }

    public canLiveOutsideStorage(): boolean { return false; }

    public get sizeOnStack(): number {
        let size = 0;
        for (const t of this.components)
            size += t ? t.sizeOnStack : 0;
        return size;
    }

    public get mobileType(): Type {
        const mobiles: Type[] = [];
        for (const c of this.components) {
            if (c) {
                const mt = c.mobileType;
                if (!mt)
                    return undefined;
                mobiles.push(mt);
            }
            else
                mobiles.push(undefined);
        }
        return new TupleType(mobiles);
    }

    /// Converts components to their temporary types and performs some wildcard matching.
    public closestTemporaryType(targetType: Type): Type {
        Debug.assert(!!targetType);
        if (targetType instanceof TupleType) {
            const targetComponents = targetType.components;
            const fillRight = targetComponents.length !== 0 && (!lastOrUndefined(targetComponents) || firstOrUndefined(targetComponents));
            const tempComponents: Type[] = [];
            for (let i = 0; i < Math.min(targetComponents.length, this.components.length); ++i) {
                const si = fillRight ? i : this.components.length - i - 1;
                const ti = fillRight ? i : targetComponents.length - i - 1;
                if (this.components[si] && targetComponents[ti]) {
                    tempComponents[ti] = this.components[si].closestTemporaryType(targetComponents[ti]);
                    Debug.assert(!!tempComponents[ti]);
                }
            }
            return new TupleType(tempComponents);
        }
    }
}

/// How this function is invoked on the EVM.
export const enum FunctionKind {
    Internal, ///< stack-call using plain JUMP
    External, ///< external call using CALL
    CallCode, ///< external call using CALLCODE, i.e. not exchanging the storage
    DelegateCall, ///< external call using DELEGATECALL, i.e. not exchanging the storage
    BareCall, ///< CALL without function hash
    BareCallCode, ///< CALLCODE without function hash
    BareDelegateCall, ///< DELEGATECALL without function hash
    Creation, ///< external call using CREATE
    Send, ///< CALL, but without data and gas
    Transfer, ///< CALL, but without data and throws on error
    SHA3, ///< SHA3
    Selfdestruct, ///< SELFDESTRUCT
    Revert, ///< REVERT
    ECRecover, ///< CALL to special contract for ecrecover
    SHA256, ///< CALL to special contract for sha256
    RIPEMD160, ///< CALL to special contract for ripemd160
    Log0,
    Log1,
    Log2,
    Log3,
    Log4,
    Event, ///< syntactic sugar for LOG*
    SetGas, ///< modify the default gas value for the function call
    SetValue, ///< modify the default value transfer for the function call
    BlockHash, ///< BLOCKHASH
    AddMod, ///< ADDMOD
    MulMod, ///< MULMOD
    ArrayPush, ///< .push() to a dynamically sized array in storage
    ByteArrayPush, ///< .push() to a dynamically sized byte array in storage
    ObjectCreation, ///< array creation using new
    Assert, ///< assert()
    Require ///< require()
};

/**
 * The type of a function, identified by its (return) parameter types.
 * @todo the return parameters should also have names, i.e. return parameters should be a struct
 * type.
 */
export class FunctionType extends Type {
    private readonly _parameterTypes: Type[];
    private readonly _returnParameterTypes: Type[];

    public get category(): TypeCategory { return TypeCategory.Function; }

    /// Creates the type of a function.
    public static newFromFunctionDefinition(fun: FunctionDefinition, isInternal = true): FunctionType {
        const kind = isInternal ? FunctionKind.Internal : FunctionKind.External;
        let stateMutability = fun.stateMutability;
        const declaration = fun;

        const params: Type[] = [];
        const paramNames: string[] = [];
        const retParams: Type[] = [];
        const retParamNames: string[] = [];

        if (isInternal && stateMutability === StateMutability.Payable)
            stateMutability = StateMutability.NonPayable;

        for (const varDecl of fun.parameters) {
            paramNames.push(varDecl.name);
            params.push(varDecl.annotation.type);
        }
        for (const varDecl of fun.returnParameters) {
            retParamNames.push(varDecl.name);
            retParams.push(varDecl.annotation.type);
        }
        return new FunctionType(
            params,
            retParams,
            paramNames,
            retParamNames,
            kind,
            false,
            stateMutability,
            declaration);
    }

    /// Creates the accessor function type of a state variable.
    public static newFromVariableDeclaration(varDecl: VariableDeclaration): FunctionType {
        const kind = FunctionKind.External;
        const stateMutability = StateMutability.View;
        const declaration = varDecl;

        const paramTypes: Type[] = [];
        const paramNames: string[] = [];
        let returnType = varDecl.annotation.type;

        while (true) {
            if (returnType instanceof MappingType) {
                paramTypes.push(returnType.keyType);
                paramNames.push("");
                returnType = returnType.valueType;
            }
            else if (returnType instanceof ArrayType) {
                if (returnType.isByteArray()) // Return byte arrays as as whole.
                    break;
                returnType = returnType.baseType;
                paramTypes.push(new IntegerType(256));
                paramNames.push("");
            }
            else
                break;
        }

        const retParams: Type[] = [];
        const retParamNames: string[] = [];
        if (returnType instanceof StructType) {
            for (const member of returnType.members(null).memberTypes) {
                Debug.assert(!!member.type);
                if (member.type.category !== TypeCategory.Mapping) {
                    const arrayType = member.type as ArrayType;
                    if (arrayType) {
                        if (!arrayType.isByteArray())
                            continue;
                    }
                    retParams.push(member.type);
                    retParamNames.push(member.name);
                }
            }
        }
        else {
            retParams.push(ReferenceType.copyForLocationIfReference(
                DataLocation.Memory,
                returnType
            ));
            retParamNames.push("");
        }

        return new FunctionType(
            paramTypes,
            retParams,
            paramNames,
            retParamNames,
            kind,
            false,
            stateMutability,
            declaration);
    }

    /// Creates the function type of an event.
    public static newFromEventDefinition(event: EventDefinition): FunctionType {
        const kind = FunctionKind.Event;
        const stateMutability = StateMutability.NonPayable;
        const declaration = event;
        const params: Type[] = [];
        const paramNames: string[] = [];
        for (const varDecl of event.parameters) {
            paramNames.push(varDecl.name);
            params.push(varDecl.annotation.type);
        }
        return new FunctionType(
            params,
            [],
            paramNames,
            [],
            kind,
            false,
            stateMutability,
            declaration);
    }

    /// Creates the type of a function type name.
    public static newFromFunctionTypeName(typeName: FunctionTypeName): FunctionType {
        const kind = typeName.visibility === Visibility.External ?
            FunctionKind.External : FunctionKind.Internal;
        if (typeName.isPayable())
            Debug.assert(kind === FunctionKind.External, "Internal payable function type used.");
        const stateMutability = typeName.stateMutability;
        const parameterTypes: Type[] = [];
        for (const t of typeName.parameterTypes) {
            Debug.assert(!!t.annotation.type, "Type not set for parameter.");
            if (kind === FunctionKind.External)
                Debug.assert(
                    t.annotation.type.canBeUsedExternally(false),
                    "Internal type used as parameter for external function."
                );
            parameterTypes.push(t.annotation.type);
        }
        const returnParameterTypes: Type[] = [];
        for (const t of typeName.returnParameterTypes) {
            Debug.assert(!!t.annotation.type, "Type not set for return parameter.");
            if (kind === FunctionKind.External)
                Debug.assert(
                    t.annotation.type.canBeUsedExternally(false),
                    "Internal type used as return parameter for external function."
                );
            returnParameterTypes.push(t.annotation.type);
        }

        return new FunctionType(
            parameterTypes,
            returnParameterTypes,
            [],
            [],
            kind,
            false,
            stateMutability);
    }

    /// Function type constructor to be used for a plain type (not derived from a declaration).
    constructor(
        parameterTypes: Type[] | string[],
        returnParameterTypes: Type[] | string[],
        private readonly _parameterNames: string[] = [],
        private readonly _returnParameterNames: string[] = [],
        public readonly kind = FunctionKind.Internal,
        private readonly _arbitraryParameters = false,
        private readonly _stateMutability = StateMutability.NonPayable,
        private readonly _declaration?: Declaration,
        public readonly gasSet = false,
        public readonly valueSet = false,
        public readonly _bound = false
    ) {
        super();
        Debug.assert(!_bound || parameterTypes.length !== 0,
            "Attempted construction of bound function without self type");

        if (parameterTypes.length !== 0 && isString(parameterTypes[0])) {
            this._parameterTypes = FunctionType.parseElementaryTypeVector(parameterTypes as string[]);
        }
        else {
            this._parameterTypes = parameterTypes as Type[];
        }
        if (returnParameterTypes.length !== 0 && isString(returnParameterTypes[0])) {
            this._returnParameterTypes = FunctionType.parseElementaryTypeVector(returnParameterTypes as string[]);
        }
        else {
            this._returnParameterTypes = returnParameterTypes as Type[];
        }
    }

    /// @returns the type of the "new Contract" function, i.e. basically the constructor.
    public static newExpressionType(contract: ContractDefinition): FunctionType {
        const constructor = contract.constructorFunction;
        const parameters: Type[] = [];
        const parameterNames: string[] = [];
        let stateMutability = StateMutability.NonPayable;

        Debug.assert(contract.contractKind !== ContractKind.Interface);

        if (constructor) {
            for (const varDecl of constructor.parameters) {
                parameterNames.push(varDecl.name);
                parameters.push(varDecl.annotation.type);
            }
            if (constructor.isPayable())
                stateMutability = StateMutability.Payable;
        }

        return new FunctionType(
            parameters,
            [new ContractType(contract)],
            parameterNames,
            [""],
            FunctionKind.Creation,
            false,
            stateMutability
        );
    }

    public get parameterTypes(): Type[] {
        if (!this._bound)
            return this._parameterTypes;
        return this._parameterTypes.slice(1);
    }
    public get parameterNames(): string[] {
        if (!this._bound)
            return this._parameterNames;
        return this._parameterNames.slice(1);
    }
    public get returnParameterTypes(): Type[] { return this._returnParameterTypes; }
    public get returnParameterNames(): string[] { return this._returnParameterNames; }

    /// @returns the "self" parameter type for a bound function
    public get selfType(): Type {
        Debug.assert(this._bound, "Function is not bound.");
        Debug.assert(this.parameterTypes.length > 0, "Function has no self type.");
        return this.parameterTypes[0];
    }

    public get identifier(): string {
        let id = "t_function_";
        switch (this.kind) {
            case FunctionKind.Internal: id += "internal"; break;
            case FunctionKind.External: id += "external"; break;
            case FunctionKind.CallCode: id += "callcode"; break;
            case FunctionKind.DelegateCall: id += "delegatecall"; break;
            case FunctionKind.BareCall: id += "barecall"; break;
            case FunctionKind.BareCallCode: id += "barecallcode"; break;
            case FunctionKind.BareDelegateCall: id += "baredelegatecall"; break;
            case FunctionKind.Creation: id += "creation"; break;
            case FunctionKind.Send: id += "send"; break;
            case FunctionKind.Transfer: id += "transfer"; break;
            case FunctionKind.SHA3: id += "sha3"; break;
            case FunctionKind.Selfdestruct: id += "selfdestruct"; break;
            case FunctionKind.Revert: id += "revert"; break;
            case FunctionKind.ECRecover: id += "ecrecover"; break;
            case FunctionKind.SHA256: id += "sha256"; break;
            case FunctionKind.RIPEMD160: id += "ripemd160"; break;
            case FunctionKind.Log0: id += "log0"; break;
            case FunctionKind.Log1: id += "log1"; break;
            case FunctionKind.Log2: id += "log2"; break;
            case FunctionKind.Log3: id += "log3"; break;
            case FunctionKind.Log4: id += "log4"; break;
            case FunctionKind.Event: id += "event"; break;
            case FunctionKind.SetGas: id += "setgas"; break;
            case FunctionKind.SetValue: id += "setvalue"; break;
            case FunctionKind.BlockHash: id += "blockhash"; break;
            case FunctionKind.AddMod: id += "addmod"; break;
            case FunctionKind.MulMod: id += "mulmod"; break;
            case FunctionKind.ArrayPush: id += "arraypush"; break;
            case FunctionKind.ByteArrayPush: id += "bytearraypush"; break;
            case FunctionKind.ObjectCreation: id += "objectcreation"; break;
            case FunctionKind.Assert: id += "assert"; break;
            case FunctionKind.Require: id += "require"; break;
            default: Debug.assert(false, "Unknown function location."); break;
        }
        id += "_" + stateMutabilityToString(this._stateMutability);
        id += identifierList(...this._parameterTypes) + "returns" + identifierList(...this._returnParameterTypes);
        if (this.gasSet)
            id += "gas";
        if (this.valueSet)
            id += "value";
        if (this._bound)
            id += "bound_to" + identifierList(this.selfType);
        return id;
    }

    public equals(other: Type): boolean {
        if (!(other instanceof FunctionType))
            return false;

        if (this.kind !== other.kind ||
            this._stateMutability !== other._stateMutability)
            return false;

        const typeCompare = (a: Type, b: Type) => a.equals(b);
        if (!arrayIsEqualTo(this._parameterTypes, other._parameterTypes, typeCompare) ||
            !arrayIsEqualTo(this._returnParameterTypes, other._returnParameterTypes, typeCompare))
            return false;

        //@todo this is ugly, but cannot be prevented right now
        if (this.gasSet !== other.gasSet || this.valueSet !== other.valueSet)
            return false;
        if (this._bound !== other._bound)
            return false;
        if (this._bound && !this.selfType.equals(other.selfType))
            return false;
        return true;
    }

    public isExplicitlyConvertibleTo(convertTo: Type): boolean {
        if (this.kind === FunctionKind.External && convertTo instanceof IntegerType) {
            if (convertTo.isAddress())
                return true;
        }
        return convertTo.category === this.category;
    }

    public unaryOperatorResult(operator: TokenName): Type | undefined {
        if (operator === TokenName.Delete)
            return new TupleType();
        return undefined;
    }
    public binaryOperatorResult(operator: TokenName, other: Type): Type | undefined {
        if (!(other instanceof FunctionType) || !(operator === TokenName.Equal || operator === TokenName.NotEqual))
            return undefined;

        if (this.kind === FunctionKind.Internal && other.kind === FunctionKind.Internal &&
            this.sizeOnStack === 1 && other.sizeOnStack === 1)
            return commonType(this, other);

        return undefined;
    }

    public get canonicalName(): string {
        Debug.assert(this.kind === FunctionKind.External);
        return "function";
    }

    public toString(short: boolean): string {
        let name = "function (";
        name += this._parameterTypes.map(t => t.toString(short)).join(",");
        name += ")";
        if (this._stateMutability !== StateMutability.NonPayable)
            name += " " + stateMutabilityToString(this._stateMutability);
        if (this.kind === FunctionKind.External)
            name += " external";
        if (this._returnParameterTypes.length !== 0) {
            name += " returns (";
            name += this._returnParameterTypes.map(t => t.toString(short)).join(",");
            name += ")";
        }
        return name;
    }

    public calldataEncodedSize(padded: boolean): number {
        let size = this.storageBytes;
        if (padded)
            size = Math.floor((size + 31) / 32) * 32;
        return size;
    }

    public canBeStored(): boolean {
        return this.kind === FunctionKind.Internal || this.kind === FunctionKind.External;
    }
    public get storageSize(): u256 {
        if (this.kind === FunctionKind.External || this.kind === FunctionKind.Internal)
            return new BN(1);
        else
            Debug.assert(false, "Storage size of non-storable function type requested.");
    }
    public get storageBytes(): number {
        if (this.kind === FunctionKind.External)
            return 20 + 4;
        else if (this.kind === FunctionKind.Internal)
            return 8; // it should really not be possible to create larger programs
        else
            Debug.assert(false, "Storage size of non-storable function type requested.");
    }

    public isValueType(): boolean { return true; }
    public canLiveOutsideStorage(): boolean { return this.kind === FunctionKind.Internal || this.kind === FunctionKind.External; }
    public get sizeOnStack(): number {
        let kind = this.kind;
        if (kind === FunctionKind.SetGas || kind === FunctionKind.SetValue) {
            Debug.assert(this._returnParameterTypes.length === 1);
            kind = (first(this._returnParameterTypes) as FunctionType).kind;
        }

        let size = 0;

        switch (kind) {
            case FunctionKind.External:
            case FunctionKind.CallCode:
            case FunctionKind.DelegateCall:
                size = 2;
                break;
            case FunctionKind.BareCall:
            case FunctionKind.BareCallCode:
            case FunctionKind.BareDelegateCall:
            case FunctionKind.Internal:
            case FunctionKind.ArrayPush:
            case FunctionKind.ByteArrayPush:
                size = 1;
                break;
            default:
                break;
        }

        if (this.gasSet)
            size++;
        if (this.valueSet)
            size++;
        if (this._bound)
            size += first(this._parameterTypes).sizeOnStack;
        return size;
    }

    public nativeMembers(_currentScope: ContractDefinition): MemberMap {
        switch (this.kind) {
            case FunctionKind.External:
            case FunctionKind.Creation:
            case FunctionKind.BareCall:
            case FunctionKind.BareCallCode:
            case FunctionKind.BareDelegateCall:
                {
                    const members: MemberMap = [];
                    if (this.kind === FunctionKind.External)
                        members.push(new Member("selector", new FixedBytesType(4)));
                    if (this.kind !== FunctionKind.BareDelegateCall) {
                        if (this.isPayable())
                            members.push(new Member(
                                "value",
                                new FunctionType(
                                    FunctionType.parseElementaryTypeVector(["uint"]),
                                    [this.copyAndSetGasOrValue(false, true)],
                                    [],
                                    [],
                                    FunctionKind.SetValue,
                                    false,
                                    StateMutability.NonPayable,
                                    undefined,
                                    this.gasSet,
                                    this.valueSet
                                )
                            ));
                    }
                    if (this.kind !== FunctionKind.Creation)
                        members.push(new Member(
                            "gas",
                            new FunctionType(
                                FunctionType.parseElementaryTypeVector(["uint"]),
                                [this.copyAndSetGasOrValue(true, false)],
                                [],
                                [],
                                FunctionKind.SetGas,
                                false,
                                StateMutability.NonPayable,
                                undefined,
                                this.gasSet,
                                this.valueSet
                            )
                        ));
                    return members;
                }
            default:
                return [];
        }
    }

    public get encodingType(): Type {
        // Only external functions can be encoded, internal functions cannot leave code boundaries.
        if (this.kind === FunctionKind.External)
            return this;
        else
            return undefined;
    }
    public interfaceType(_inLibrary: boolean): Type | undefined {
        if (this.kind === FunctionKind.External)
            return this;
        else
            return undefined;
    }

    /// @returns TypePointer of a new FunctionType object. All input/return parameters are an
    /// appropriate external types (i.e. the interfaceType()s) of input/return parameters of
    /// current function.
    /// @returns an empty shared pointer if one of the input/return parameters does not have an
    /// external type.
    public get interfaceFunctionType(): FunctionType | undefined {
        // Note that m_declaration might also be a state variable!
        Debug.assert(!!this._declaration, "Declaration needed to determine interface function type.");
        const isLibraryFunction = (this._declaration.scope as ContractDefinition).isLibrary();

        const paramTypes: Type[] = [];
        const retParamTypes: Type[] = [];

        for (const type of this._parameterTypes) {
            const ext = type.interfaceType(isLibraryFunction);
            if (ext)
                paramTypes.push(ext);
            else
                return undefined;
        }
        for (const type of this._returnParameterTypes) {
            const ext = type.interfaceType(isLibraryFunction);
            if (ext)
                retParamTypes.push(ext);
            else
                return undefined;
        }
        const variable = this._declaration as VariableDeclaration;
        if (variable && retParamTypes.length === 0)
            return undefined;

        return new FunctionType(
            paramTypes,
            retParamTypes,
            this._parameterNames,
            this._returnParameterNames,
            this.kind,
            this._arbitraryParameters,
            this._stateMutability,
            this._declaration
        );
    }

    /// @returns true if this function can take the given argument types (possibly
    /// after implicit conversion).
    /// @param _selfType if the function is bound, this has to be supplied and is the type of the
    /// expression the function is called on.
    public canTakeArguments(argumentTypes: Type[], selfType?: Type): boolean {
        Debug.assert(!this._bound || !!selfType);
        if (this._bound && !selfType.isImplicitlyConvertibleTo(this.selfType))
            return false;
        const paramTypes = this._parameterTypes;
        if (this.takesArbitraryParameters())
            return true;
        else
            return arrayIsEqualTo(argumentTypes, paramTypes,
                (argumentType, parameterType) => argumentType.isImplicitlyConvertibleTo(parameterType));
    }
    /// @returns true if the types of parameters are equal (does't check return parameter types)
    public hasEqualArgumentTypes(other: FunctionType): boolean {
        return arrayIsEqualTo(this._parameterTypes, other._parameterTypes, (a, b) => a.equals(b));
    }

    /// @returns true if the ABI is used for this call (only meaningful for external calls)
    public isBareCall(): boolean {
        switch (this.kind) {
            case FunctionKind.BareCall:
            case FunctionKind.BareCallCode:
            case FunctionKind.BareDelegateCall:
            case FunctionKind.ECRecover:
            case FunctionKind.SHA256:
            case FunctionKind.RIPEMD160:
                return true;
            default:
                return false;
        }
    }

    /// @returns the external signature of this function type given the function name
    public get externalSignature(): string {
        Debug.assert(!!this._declaration, "External signature of function needs declaration");
        Debug.assert(this._declaration.name !== "", "Fallback function has no signature.");

        const inLibrary = (this._declaration.scope as ContractDefinition).isLibrary();
        const external = this.interfaceFunctionType;
        Debug.assert(!!external, "External function type requested.");
        const parameterTypes = external.parameterTypes;
        const typeStrings = parameterTypes.map(t => {
            Debug.assert(!!t, "Parameter should have external type.");
            let typeName = t.signatureInExternalFunction(inLibrary);
            if (inLibrary && t.dataStoredIn(DataLocation.Storage))
                typeName += " storage";
            return typeName;
        });
        return this._declaration.name + "(" + typeStrings.join(",") + ")";
    }

    /// @returns the external identifier of this function (the hash of the signature).
    public get externalIdentifier(): string {
        return util.sha3(this.externalSignature).toString("hex");
    }

    public get declaration(): Declaration {
        Debug.assert(!!this._declaration, "Requested declaration from a FunctionType that has none");
        return this._declaration;
    }

    public hasDeclaration(): boolean { return !!this._declaration; }

    /// @returns true if the the result of this function only depends on its arguments
    /// and it does not modify the state.
    /// Currently, this will only return true for internal functions like keccak and ecrecover.
    public isPure(): boolean {
        // FIXME: replace this with m_stateMutability === StateMutability::Pure once
        //        the callgraph analyzer is in place
        return this.kind === FunctionKind.SHA3 ||
            this.kind === FunctionKind.ECRecover ||
            this.kind === FunctionKind.SHA256 ||
            this.kind === FunctionKind.RIPEMD160 ||
            this.kind === FunctionKind.AddMod ||
            this.kind === FunctionKind.MulMod ||
            this.kind === FunctionKind.ObjectCreation;
    }

    public isPayable(): boolean { return this._stateMutability === StateMutability.Payable; }

    /// @return A shared pointer of an ASTString.
    /// Can contain a nullptr in which case indicates absence of documentation
    public get documentation(): string {
        if (this._declaration instanceof Documented) {
            const fun = this._declaration;
            if (fun)
                return fun.documentation();
        }

        return undefined;
    }

    /// true iff arguments are to be padded to multiples of 32 bytes for external calls
    public padArguments(): boolean {
        return !(this.kind === FunctionKind.SHA3
            || this.kind === FunctionKind.SHA256
            || this.kind === FunctionKind.RIPEMD160);
    }
    public takesArbitraryParameters(): boolean { return this._arbitraryParameters; }

    /// @returns a copy of this type, where gas or value are set manually. This will never set one
    /// of the parameters to fals.
    public copyAndSetGasOrValue(setGas: boolean, setValue: boolean): Type {
        return new FunctionType(
            this._parameterTypes,
            this._returnParameterTypes,
            this._parameterNames,
            this._returnParameterNames,
            this.kind,
            this._arbitraryParameters,
            this._stateMutability,
            this._declaration,
            this.gasSet || setGas,
            this.valueSet || setValue,
            this._bound
        );
    }

    /// @returns a copy of this function type where all return parameters of dynamic size are
    /// removed and the location of reference types is changed from CallData to Memory.
    /// This is needed if external functions are called on other contracts, as they cannot return
    /// dynamic values.
    /// Returns empty shared pointer on a failure. Namely, if a bound function has no parameters.
    /// @param _inLibrary if true, uses DelegateCall as location.
    /// @param _bound if true, the arguments are placed as `arg1.functionName(arg2, ..., argn)`.
    public asMemberFunction(inLibrary: boolean, bound = false): FunctionType | undefined {
        if (bound && this._parameterTypes.length === 0)
            return undefined;

        const parameterTypes: Type[] = [];
        for (const t of this._parameterTypes) {
            const refType = t as ReferenceType;
            if (refType && refType.location === DataLocation.CallData)
                parameterTypes.push(refType.copyForLocation(DataLocation.Memory, true));
            else
                parameterTypes.push(t);
        }

        let kind = this.kind;
        if (inLibrary) {
            Debug.assert(!!this._declaration, "Declaration has to be available.");
            if (!this._declaration.isPublic())
                kind = FunctionKind.Internal; // will be inlined
            else
                kind = FunctionKind.DelegateCall;
        }

        const returnParameterTypes = this._returnParameterTypes.slice();
        if (kind !== FunctionKind.Internal) {
            // Alter dynamic types to be non-accessible.
            for (let i = 0; i < returnParameterTypes.length; ++i) {
                if (returnParameterTypes[i].isDynamicallySized())
                    returnParameterTypes[i] = new InaccessibleDynamicType();
            }
        }

        return new FunctionType(
            parameterTypes,
            returnParameterTypes,
            this._parameterNames,
            this._returnParameterNames,
            kind,
            this._arbitraryParameters,
            this._stateMutability,
            this._declaration,
            this.gasSet,
            this.valueSet,
            bound
        );
    }

    private static parseElementaryTypeVector(typeNames: string[]): Type[] {
        const types: Type[] = [];
        for (const typeName of typeNames)
            types.push(Type.fromElementaryTypeName(typeName));
        return types;
    }
}

/**
 * The type of a mapping, there is one distinct type per key/value type pair.
 * Mappings always occupy their own storage slot, but do not actually use it.
 */
export class MappingType extends Type {
    public get category(): TypeCategory { return TypeCategory.Mapping; }

    constructor(public readonly keyType: Type, public readonly valueType: Type) {
        super();
    }

    public get identifier(): string {
        return "t_mapping" + identifierList(this.keyType, this.valueType);
    }

    public equals(other: Type): boolean {
        return other instanceof MappingType &&
            other.keyType.equals(this.keyType) &&
            other.valueType.equals(this.valueType);
    }

    public toString(short: boolean): string {
        return "mapping(" + this.keyType.toString(short) + " => " + this.valueType.toString(short) + ")";
    }

    public get canonicalName(): string {
        return "mapping(" + this.keyType.canonicalName + " => " + this.valueType.canonicalName + ")";
    }

    public canLiveOutsideStorage(): boolean { return false; }

    public binaryOperatorResult(_operator: TokenName, _other: Type): Type | undefined { return undefined; }

    public get encodingType(): Type {
        return new IntegerType(256);
    }
    public interfaceType(inLibrary: boolean): Type | undefined {
        return inLibrary ? this : undefined;
    }

    public dataStoredIn(location: DataLocation): boolean { return location === DataLocation.Storage; }
}

/**
 * The type of a type reference. The type of "uint32" when used in "a = uint32(2)" is an example
 * of a TypeType.
 * For super contracts or libraries, this has members directly.
 */
export class TypeType extends Type {
    public get category(): TypeCategory { return TypeCategory.TypeType; }
    constructor(public readonly actualType: Type) {
        super();
    }

    public binaryOperatorResult(_operator: TokenName, _other: Type): Type | undefined { return undefined; }

    public get identifier(): string {
        return "t_type" + identifierList(this.actualType);
    }

    public equals(other: Type): boolean {
        return other instanceof TypeType && this.actualType.equals(other.actualType);
    }

    public canBeStored(): boolean { return false; }
    public get storageSize(): u256 {
        Debug.assert(false, "Storage size of non-storable type type requested.");
        return new BN(0);
    }
    public canLiveOutsideStorage(): boolean { return false; }
    public get sizeOnStack(): number {
        if (this.actualType instanceof ContractType) {
            const contractType = this.actualType;
            if (contractType.contractDefinition.isLibrary())
                return 1;
        }
        return 0;
    }

    public toString(short: boolean) {
        return "type(" + this.actualType.toString(short) + ")";
    }

    public nativeMembers(currentScope: ContractDefinition): MemberMap {
        const members: MemberMap = [];
        if (this.actualType instanceof ContractType) {
            const contract = this.actualType.contractDefinition;
            let isBase = false;
            if (currentScope) {
                const currentBases = currentScope.annotation.linearizedBaseContracts;
                isBase = contains(currentBases, contract);
            }
            if (contract.isLibrary()) {
                for (const fun of contract.definedFunctions) {
                    if (fun.isVisibleInDerivedContracts())
                        members.push(new Member(
                            fun.name,
                            FunctionType.newFromFunctionDefinition(fun).asMemberFunction(true),
                            fun
                        ));
                }
            }
            if (isBase) {
                // We are accessing the type of a base contract, so add all public and protected
                // members. Note that this does not add inherited functions on purpose.
                for (const decl of contract.inheritableMembers())
                    members.push(new Member(decl.name, decl.type, decl));
            }
            else {
                for (const stru of contract.definedStructs)
                    members.push(new Member(stru.name, stru.type, stru));
                for (const enu of contract.definedEnums)
                    members.push(new Member(enu.name, enu.type, enu));
            }
        }
        else if (this.actualType instanceof EnumType) {
            const enumDef = this.actualType.enumDefinition;
            const enumType = new EnumType(enumDef);
            for (const enumValue of enumDef.members)
                members.push(new Member(enumValue.name, enumType));
        }
        return members;
    }
}

/**
 * The type of a function modifier. Not used for anything for now.
 */
export class ModifierType extends Type {
    private parameterTypes: Type[] = [];

    public get category(): TypeCategory { return TypeCategory.Modifier; }
    constructor(modifier: ModifierDefinition) {
        super();
        for (const varDecl of modifier.parameters)
            this.parameterTypes.push(varDecl.annotation.type);
    }

    public binaryOperatorResult(_operator: TokenName, _type: Type): Type | undefined { return undefined; }

    public canBeStored(): boolean { return false; }

    public get storageSize(): u256 {
        Debug.assert(false, "Storage size of non-storable type type requested.");
        return new BN(0);
    }

    public canLiveOutsideStorage(): boolean { return false; }
    public get sizeOnStack(): number { return 0; }

    public get identifier(): string {
        return "t_modifier" + identifierList(...this.parameterTypes);
    }

    public equals(other: Type): boolean {
        return other instanceof ModifierType &&
            arrayIsEqualTo(this.parameterTypes, other.parameterTypes, (a, b) => a.equals(b));
    }

    public toString(short: boolean): string {
        let name = "modifier (";
        name += this.parameterTypes.map(t => t.toString(short)).join(",");
        return name + ")";
    }
}

/**
 * Special type for imported modules. These mainly give access to their scope via members.
 */
export class ModuleType extends Type {
    public get category(): TypeCategory { return TypeCategory.Module; }

    constructor(private readonly sourceUnit: SourceUnit) {
        super();
    }

    public binaryOperatorResult(_operator: TokenName, _type: Type): Type | undefined { return undefined; }

    public get identifier(): string {
        return "t_module_" + this.sourceUnit.id;
    }

    public equals(other: Type): boolean {
        return other instanceof ModuleType &&
            this.sourceUnit === other.sourceUnit;
    }

    public canBeStored(): boolean { return false; }
    public canLiveOutsideStorage(): boolean { return true; }
    public get sizeOnStack(): number { return 0; }

    public nativeMembers(_contract: ContractDefinition): MemberMap {
        const symbols: MemberMap = [];
        this.sourceUnit.annotation.exportedSymbols.forEach((declarations, name) => {
            for (const declaration of declarations)
                symbols.push(new Member(name, declaration.type, declaration));
        });
        return symbols;
    }

    public toString(_short: boolean): string {
        return "module \"" + this.sourceUnit.annotation.path + "\"";
    }
}

export const enum MagicKind { Block, Message, Transaction }

/**
 * Special type for magic variables (block, msg, tx), similar to a struct but without any reference
 * (it always references a global singleton by name).
 */
export class MagicType extends Type {
    public get category(): TypeCategory { return TypeCategory.Magic; }

    constructor(public readonly kind: MagicKind) {
        super();
    }

    public binaryOperatorResult(_operator: TokenName, _other: Type): Type | undefined {
        return undefined;
    }

    public get identifier(): string {
        switch (this.kind) {
            case MagicKind.Block:
                return "t_magic_block";
            case MagicKind.Message:
                return "t_magic_message";
            case MagicKind.Transaction:
                return "t_magic_transaction";
            default:
                Debug.assert(false, "Unknown kind of magic");
        }
        return "";
    }

    public equals(other: Type): boolean {
        return other instanceof MagicType && other.kind === this.kind;
    }

    public canBeStored(): boolean { return false; }
    public canLiveOutsideStorage() { return true; }
    public get sizeOnStack(): number { return 0; }

    public nativeMembers(_contract: ContractDefinition): MemberMap {
        switch (this.kind) {
            case MagicKind.Block:
                return [
                    new Member("coinbase", new IntegerType(160, IntegerTypeModifier.Address)),
                    new Member("timestamp", new IntegerType(256)),
                    new Member("blockhash", new FunctionType(["uint"], ["bytes32"], [], [], FunctionKind.BlockHash, false, StateMutability.View)),
                    new Member("difficulty", new IntegerType(256)),
                    new Member("number", new IntegerType(256)),
                    new Member("gaslimit", new IntegerType(256))
                ];
            case MagicKind.Message:
                return [
                    new Member("sender", new IntegerType(160, IntegerTypeModifier.Address)),
                    new Member("gas", new IntegerType(256)),
                    new Member("value", new IntegerType(256)),
                    new Member("data", ArrayType.newBytesOrString(DataLocation.CallData)),
                    new Member("sig", new FixedBytesType(4))
                ];
            case MagicKind.Transaction:
                return [
                    new Member("origin", new IntegerType(160, IntegerTypeModifier.Address)),
                    new Member("gasprice", new IntegerType(256))
                ];
            default:
                Debug.assert(false, "Unknown kind of magic.");
        }
    }

    public toString(_short: boolean): string {
        switch (this.kind) {
            case MagicKind.Block:
                return "block";
            case MagicKind.Message:
                return "msg";
            case MagicKind.Transaction:
                return "tx";
            default:
                Debug.assert(false, "Unknown kind of magic.");
        }
    }
}

/**
 * Special type that is used for dynamic types in returns from external function calls
 * (The EVM currently cannot access dynamically-sized return values).
 */
export class InaccessibleDynamicType extends Type {
    public get category(): TypeCategory { return TypeCategory.InaccessibleDynamic; }

    public get identifier(): string { return "t_inaccessible"; }
    public isImplicitlyConvertibleTo(_other: Type): boolean { return false; }
    public isExplicitlyConvertibleTo(_other: Type): boolean { return false; }
    public binaryOperatorResult(_operator: TokenName, _other: Type): Type | undefined { return undefined; }
    public calldataEncodedSize(_padded: boolean): number { return 32; }
    public canBeStored(): boolean { return false; }
    public canLiveOutsideStorage(): boolean { return false; }
    public isValueType(): boolean { return true; }
    public get sizeOnStack(): number { return 1; }
    public toString(_short: boolean): string { return "inaccessible dynamic type"; }
    public get decodingType(): Type { return new IntegerType(256); }
}

function isValidShiftAndAmountType(operator: TokenName, shiftAmountType: Type): boolean {
    // Disable >>> here.
    if (operator === TokenName.SHR)
        return false;
    else if (shiftAmountType instanceof IntegerType)
        return !shiftAmountType.isAddress();
    else if (shiftAmountType instanceof RationalNumberType)
        return shiftAmountType.integerType && !shiftAmountType.integerType.isSigned();
    else
        return false;
}

function identifier(type: Type): string {
    return type ? type.identifier : "";
}

function identifierList(...types: Type[]): string {
    return "$_" + types.map(identifier).join("_$_") + "_$";
}

function parenthesizeIdentifier(internal: string): string {
    return "$_" + internal + "_$";
}

function parenthesizeUserIdentifier(internal: string): string {
    // Replace $ with $$$.
    return parenthesizeIdentifier(internal.replace(/\$/g, "$$$$$$"));
}

/// Determine bytes required to encode the given integer value. @returns 0 if @a n is zero.
function bytesRequired(n: number | BN): number {
    let i = 0;
    if (n instanceof BN) {
        for (; !n.isZero(); ++i, n = n.div(256).floor());
    } else {
        for (; n !== 0; ++i, n >>= 8) { }
    }
    return i;
}

function stringToCharCodes(s: string): number[] {
    return s.split("").map(c => c.charCodeAt(0));
}
