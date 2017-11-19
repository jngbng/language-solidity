import { Token } from "../parsing/token";
import {
    ASTNode,
    ArrayTypeName,
    Assignment,
    BinaryOperation,
    Block,
    Break,
    Conditional,
    Continue,
    ContractDefinition,
    ElementaryTypeName,
    ElementaryTypeNameExpression,
    EnumDefinition,
    EnumValue,
    EventDefinition,
    ExpressionStatement,
    ForStatement,
    FunctionCall,
    FunctionDefinition,
    FunctionTypeName,
    Identifier,
    IfStatement,
    ImportDirective,
    IndexAccess,
    InheritanceSpecifier,
    Literal,
    Mapping,
    MemberAccess,
    ModifierDefinition,
    ModifierInvocation,
    NewExpression,
    ParameterList,
    PlaceholderStatement,
    PragmaDirective,
    Return,
    StateMutability,
    StructDefinition,
    Throw,
    TupleExpression,
    UnaryOperation,
    UserDefinedTypeName,
    VariableDeclaration,
    VariableDeclarationStatement,
    WhileStatement
} from "./ast";
import { ASTVisitor } from "./astVisitor";

export interface LineWriter {
    writeLine(s: string): void;
}

/**
 * Visitor interface for the abstract syntax tree. This class is tightly bound to the
 * implementation of @ref ASTNode::accept and its overrides. After a call to
 * @ref ASTNode::accept, the function visit for the appropriate parameter is called and then
 * (if it returns true) this continues recursively for all child nodes in document order
 * (there is an exception for contracts). After all child nodes have been visited, endVisit is
 * called for the node.
 */
export class ASTPrinter extends ASTVisitor {
    private _indentation = 0;
    private writer: LineWriter;

    constructor(private readonly ast: ASTNode, private readonly source = "") {
        super();
        this.ast = ast;
    }

    /// Output the string representation of the AST to _stream.
    public print(writer: LineWriter) {
        this.writer = writer;
        this.ast.accept(this);
        this.writer = null;
    }

    public visitPragmaDirective(node: PragmaDirective): boolean {
        this.writeLine("PragmaDirective");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitAssignment(node: Assignment): boolean {
        this.writeLine(`Assignment using operator ${Token.tokenToString(node.assignmentOperator)}`);
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitConditional(node: Conditional): boolean {
        this.writeLine("Conditional");
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitIndexAccess(node: IndexAccess): boolean {
        this.writeLine("IndexAccess");
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitMemberAccess(node: MemberAccess): boolean {
        this.writeLine("MemberAccess to member " + node.memberName);
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitNewExpression(node: NewExpression): boolean {
        this.writeLine("NewExpression");
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitFunctionCall(node: FunctionCall): boolean {
        this.writeLine("FunctionCall");
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitUnaryOperation(node: UnaryOperation): boolean {
        this.writeLine(`UnaryOperation (${node.isPrefixOperation() ? "prefix" : "postfix"}) ${Token.tokenToString(node.operator)})`);
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitBinaryOperation(node: BinaryOperation): boolean {
        this.writeLine(`BinaryOperation using operator ${Token.tokenToString(node.operator)}`);
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitTupleExpression(node: TupleExpression): boolean {
        this.writeLine("TupleExpression");
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitIdentifier(node: Identifier): boolean {
        this.writeLine("Identifier " + node.name);
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitLiteral(node: Literal): boolean {
        let tokenString = Token.tokenToString(node.token);
        if (!tokenString)
            tokenString = "[no token]";
        this.writeLine(`Literal, token: ${tokenString} value: ${node.value}`);
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitElementaryTypeNameExpression(node: ElementaryTypeNameExpression): boolean {
        this.writeLine("ElementaryTypeNameExpression " + node.typeName.toString());
        this.printType(node);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitFunctionDefinition(node: FunctionDefinition): boolean {
        this.writeLine(`FunctionDefinition "${node.name}" ${node.isPublic() ? " - public" : ""} ${node.stateMutability === StateMutability.View ? " - const" : ""}`);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitVariableDeclaration(node: VariableDeclaration): boolean {
        this.writeLine(`VariableDeclaration "${node.name}"`);
        this.writeLine(
            // FIXME: Print the type.
            this.indentation() + "  Type unknown."
        );
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitModifierInvocation(node: ModifierInvocation): boolean {
        this.writeLine(`ModifierInvocation "${node.name.name}"`);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitParameterList(node: ParameterList): boolean {
        this.writeLine("ParameterList");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitInheritanceSpecifier(node: InheritanceSpecifier): boolean {
        this.writeLine("InheritanceSpecifier");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitBlock(node: Block): boolean {
        this.writeLine("Block");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitPlaceholderStatement(node: PlaceholderStatement): boolean {
        this.writeLine("PlaceholderStatement");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitIfStatement(node: IfStatement): boolean {
        this.writeLine("IfStatement");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitWhileStatement(node: WhileStatement): boolean {
        this.writeLine(node.isDoWhile() ? "DoWhileStatement" : "WhileStatement");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitForStatement(node: ForStatement): boolean {
        this.writeLine("ForStatement");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitContinue(node: Continue): boolean {
        this.writeLine("Continue");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitBreak(node: Break): boolean {
        this.writeLine("Break");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitReturn(node: Return): boolean {
        this.writeLine("Return");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitThrow(node: Throw): boolean {
        this.writeLine("Throw");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitVariableDeclarationStatement(node: VariableDeclarationStatement): boolean {
        this.writeLine("VariableDeclarationStatement");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitExpressionStatement(node: ExpressionStatement): boolean {
        this.writeLine("ExpressionStatement");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitContractDefinition(node: ContractDefinition): boolean {
        this.writeLine(`ContractDefinition "${node.name}"`);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitImportDirective(node: ImportDirective): boolean {
        this.writeLine(`ImportDirective "${node.path}"`);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitEventDefinition(node: EventDefinition): boolean {
        this.writeLine(`EventDefinition "${node.name}"`);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitModifierDefinition(node: ModifierDefinition): boolean {
        this.writeLine(`ModifierDefinition "${node.name}"`);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitStructDefinition(node: StructDefinition): boolean {
        this.writeLine(`StructDefinition "${node.name}"`);
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitEnumDefinition(node: EnumDefinition): boolean {
        this.writeLine(`EnumDefinition "${node.name}"`);
        return this.goDeeper();
    }

    public visitEnumValue(node: EnumValue): boolean {
        this.writeLine(`EnumValue "${node.name}"`);
        return this.goDeeper();
    }

    public visitElementaryTypeName(node: ElementaryTypeName): boolean {
        this.writeLine("ElementaryTypeName " + node.typeName.toString());
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitArrayTypeName(node: ArrayTypeName): boolean {
        this.writeLine("ArrayTypeName");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitMapping(node: Mapping): boolean {
        this.writeLine("Mapping");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitFunctionTypeName(node: FunctionTypeName): boolean {
        this.writeLine("FunctionTypeName");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public visitUserDefinedTypeName(node: UserDefinedTypeName): boolean {
        this.writeLine("UserDefinedTypeName \"" + node.namePath.join(".") + "\"");
        this.printSourcePart(node);
        return this.goDeeper();
    }

    public endVisitPragmaDirective(_node: PragmaDirective) { this._indentation--; }
    public endVisitAssignment(_node: Assignment) { this._indentation--; }
    public endVisitConditional(_node: Conditional) { this._indentation--; }
    public endVisitIndexAccess(_node: IndexAccess) { this._indentation--; }
    public endVisitMemberAccess(_node: MemberAccess) { this._indentation--; }
    public endVisitNewExpression(_node: NewExpression) { this._indentation--; }
    public endVisitFunctionCall(_node: FunctionCall) { this._indentation--; }
    public endVisitUnaryOperation(_node: UnaryOperation) { this._indentation--; }
    public endVisitBinaryOperation(_node: BinaryOperation) { this._indentation--; }
    public endVisitTupleExpression(_node: TupleExpression) { this._indentation--; }
    public endVisitIdentifier(_node: Identifier) { this._indentation--; }
    public endVisitLiteral(_node: Literal) { this._indentation--; }
    public endVisitElementaryTypeNameExpression(_node: ElementaryTypeNameExpression) { this._indentation--; }
    public endVisitFunctionDefinition(_node: FunctionDefinition) { this._indentation--; }
    public endVisitVariableDeclaration(_node: VariableDeclaration) { this._indentation--; }
    public endVisitModifierInvocation(_node: ModifierInvocation) { this._indentation--; }
    public endVisitParameterList(_node: ParameterList) { this._indentation--; }
    public endVisitInheritanceSpecifier(_node: InheritanceSpecifier) { this._indentation--; }
    public endVisitBlock(_node: Block) { this._indentation--; }
    public endVisitPlaceholderStatement(_node: PlaceholderStatement) { this._indentation--; }
    public endVisitIfStatement(_node: IfStatement) { this._indentation--; }
    public endVisitWhileStatement(_node: WhileStatement) { this._indentation--; }
    public endVisitForStatement(_node: ForStatement) { this._indentation--; }
    public endVisitContinue(_node: Continue) { this._indentation--; }
    public endVisitBreak(_node: Break) { this._indentation--; }
    public endVisitReturn(_node: Return) { this._indentation--; }
    public endVisitThrow(_node: Throw) { this._indentation--; }
    public endVisitVariableDeclarationStatement(_node: VariableDeclarationStatement) { this._indentation--; }
    public endVisitExpressionStatement(_node: ExpressionStatement) { this._indentation--; }
    public endVisitContractDefinition(_node: ContractDefinition) { this._indentation--; }
    public endVisitImportDirective(_node: ImportDirective) { this._indentation--; }
    public endVisitEventDefinition(_node: EventDefinition) { this._indentation--; }
    public endVisitModifierDefinition(_node: ModifierDefinition) { this._indentation--; }
    public endVisitStructDefinition(_node: StructDefinition) { this._indentation--; }
    public endVisitEnumDefinition(_node: EnumDefinition) { this._indentation--; }
    public endVisitEnumValue(_node: EnumValue) { this._indentation--; }
    public endVisitElementaryTypeName(_node: ElementaryTypeName) { this._indentation--; }
    public endVisitArrayTypeName(_node: ArrayTypeName) { this._indentation--; }
    public endVisitMapping(_node: Mapping) { this._indentation--; }
    public endVisitFunctionTypeName(_node: FunctionTypeName) { this._indentation--; }
    public endVisitUserDefinedTypeName(_node: UserDefinedTypeName) { this._indentation--; }

    private indentation(): string {
        return " ".repeat(this._indentation * 2);
    }

    private printType(_node: ASTNode) {
        // FIXME: Print the type.
    }

    private printSourcePart(node: ASTNode) {
        if (this.source !== "") {
            const location = node.location;
            this.writeLine("  Source: " +
                JSON.stringify(this.source.substring(location.start, location.end))
            );
        }
    }

    private writeLine(s: string) {
        this.writer.writeLine(this.indentation() + s);
    }

    private goDeeper(): boolean {
        this._indentation++;
        return true;
    }
}
