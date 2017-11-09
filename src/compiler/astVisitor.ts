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
    SourceUnit,
    StructDefinition,
    Throw,
    TupleExpression,
    UnaryOperation,
    UserDefinedTypeName,
    UsingForDirective,
    VariableDeclaration,
    VariableDeclarationStatement,
    WhileStatement
} from "./ast";

/**
 * Visitor interface for the abstract syntax tree. This class is tightly bound to the
 * implementation of @ref ASTNode::accept and its overrides. After a call to
 * @ref ASTNode::accept, the function visit for the appropriate parameter is called and then
 * (if it returns true) this continues recursively for all child nodes in document order
 * (there is an exception for contracts). After all child nodes have been visited, endVisit is
 * called for the node.
 */
export class ASTVisitor {
    public visitSourceUnit(node: SourceUnit): boolean { return this.visitNode(node); }
    public visitPragmaDirective(node: PragmaDirective): boolean { return this.visitNode(node); }
    public visitAssignment(node: Assignment): boolean { return this.visitNode(node); }
    public visitConditional(node: Conditional): boolean { return this.visitNode(node); }
    public visitIndexAccess(node: IndexAccess): boolean { return this.visitNode(node); }
    public visitMemberAccess(node: MemberAccess): boolean { return this.visitNode(node); }
    public visitNewExpression(node: NewExpression): boolean { return this.visitNode(node); }
    public visitFunctionCall(node: FunctionCall): boolean { return this.visitNode(node); }
    public visitUnaryOperation(node: UnaryOperation): boolean { return this.visitNode(node); }
    public visitBinaryOperation(node: BinaryOperation): boolean { return this.visitNode(node); }
    public visitTupleExpression(node: TupleExpression): boolean { return this.visitNode(node); }
    public visitIdentifier(node: Identifier): boolean { return this.visitNode(node); }
    public visitLiteral(node: Literal): boolean { return this.visitNode(node); }
    public visitElementaryTypeNameExpression(node: ElementaryTypeNameExpression): boolean { return this.visitNode(node); }
    public visitFunctionDefinition(node: FunctionDefinition): boolean { return this.visitNode(node); }
    public visitVariableDeclaration(node: VariableDeclaration): boolean { return this.visitNode(node); }
    public visitModifierInvocation(node: ModifierInvocation): boolean { return this.visitNode(node); }
    public visitParameterList(node: ParameterList): boolean { return this.visitNode(node); }
    public visitInheritanceSpecifier(node: InheritanceSpecifier): boolean { return this.visitNode(node); }
    public visitBlock(node: Block): boolean { return this.visitNode(node); }
    public visitPlaceholderStatement(node: PlaceholderStatement): boolean { return this.visitNode(node); }
    public visitIfStatement(node: IfStatement): boolean { return this.visitNode(node); }
    public visitWhileStatement(node: WhileStatement): boolean { return this.visitNode(node); }
    public visitForStatement(node: ForStatement): boolean { return this.visitNode(node); }
    public visitContinue(node: Continue): boolean { return this.visitNode(node); }
    public visitBreak(node: Break): boolean { return this.visitNode(node); }
    public visitReturn(node: Return): boolean { return this.visitNode(node); }
    public visitThrow(node: Throw): boolean { return this.visitNode(node); }
    public visitVariableDeclarationStatement(node: VariableDeclarationStatement): boolean { return this.visitNode(node); }
    public visitExpressionStatement(node: ExpressionStatement): boolean { return this.visitNode(node); }
    public visitContractDefinition(node: ContractDefinition): boolean { return this.visitNode(node); }
    public visitImportDirective(node: ImportDirective): boolean { return this.visitNode(node); }
    public visitEventDefinition(node: EventDefinition): boolean { return this.visitNode(node); }
    public visitModifierDefinition(node: ModifierDefinition): boolean { return this.visitNode(node); }
    public visitUsingForDirective(node: UsingForDirective): boolean { return this.visitNode(node); }
    public visitStructDefinition(node: StructDefinition): boolean { return this.visitNode(node); }
    public visitEnumDefinition(node: EnumDefinition): boolean { return this.visitNode(node); }
    public visitEnumValue(node: EnumValue): boolean { return this.visitNode(node); }
    public visitElementaryTypeName(node: ElementaryTypeName): boolean { return this.visitNode(node); }
    public visitArrayTypeName(node: ArrayTypeName): boolean { return this.visitNode(node); }
    public visitMapping(node: Mapping): boolean { return this.visitNode(node); }
    public visitFunctionTypeName(node: FunctionTypeName): boolean { return this.visitNode(node); }
    public visitUserDefinedTypeName(node: UserDefinedTypeName): boolean { return this.visitNode(node); }

    public endVisitSourceUnit(node: SourceUnit) { this.endVisitNode(node); }
    public endVisitPragmaDirective(node: PragmaDirective) { this.endVisitNode(node); }
    public endVisitAssignment(node: Assignment) { this.endVisitNode(node); }
    public endVisitConditional(node: Conditional) { this.endVisitNode(node); }
    public endVisitIndexAccess(node: IndexAccess) { this.endVisitNode(node); }
    public endVisitMemberAccess(node: MemberAccess) { this.endVisitNode(node); }
    public endVisitNewExpression(node: NewExpression) { this.endVisitNode(node); }
    public endVisitFunctionCall(node: FunctionCall) { this.endVisitNode(node); }
    public endVisitUnaryOperation(node: UnaryOperation) { this.endVisitNode(node); }
    public endVisitBinaryOperation(node: BinaryOperation) { this.endVisitNode(node); }
    public endVisitTupleExpression(node: TupleExpression) { this.endVisitNode(node); }
    public endVisitIdentifier(node: Identifier) { this.endVisitNode(node); }
    public endVisitLiteral(node: Literal) { this.endVisitNode(node); }
    public endVisitElementaryTypeNameExpression(node: ElementaryTypeNameExpression) { this.endVisitNode(node); }
    public endVisitFunctionDefinition(node: FunctionDefinition) { this.endVisitNode(node); }
    public endVisitVariableDeclaration(node: VariableDeclaration) { this.endVisitNode(node); }
    public endVisitModifierInvocation(node: ModifierInvocation) { this.endVisitNode(node); }
    public endVisitParameterList(node: ParameterList) { this.endVisitNode(node); }
    public endVisitInheritanceSpecifier(node: InheritanceSpecifier) { this.endVisitNode(node); }
    public endVisitBlock(node: Block) { this.endVisitNode(node); }
    public endVisitPlaceholderStatement(node: PlaceholderStatement) { this.endVisitNode(node); }
    public endVisitIfStatement(node: IfStatement) { this.endVisitNode(node); }
    public endVisitWhileStatement(node: WhileStatement) { this.endVisitNode(node); }
    public endVisitForStatement(node: ForStatement) { this.endVisitNode(node); }
    public endVisitContinue(node: Continue) { this.endVisitNode(node); }
    public endVisitBreak(node: Break) { this.endVisitNode(node); }
    public endVisitReturn(node: Return) { this.endVisitNode(node); }
    public endVisitThrow(node: Throw) { this.endVisitNode(node); }
    public endVisitVariableDeclarationStatement(node: VariableDeclarationStatement) { this.endVisitNode(node); }
    public endVisitExpressionStatement(node: ExpressionStatement) { this.endVisitNode(node); }
    public endVisitContractDefinition(node: ContractDefinition) { this.endVisitNode(node); }
    public endVisitImportDirective(node: ImportDirective) { this.endVisitNode(node); }
    public endVisitEventDefinition(node: EventDefinition) { this.endVisitNode(node); }
    public endVisitModifierDefinition(node: ModifierDefinition) { this.endVisitNode(node); }
    public endVisitUsingForDirective(node: UsingForDirective) { this.endVisitNode(node); }
    public endVisitStructDefinition(node: StructDefinition) { this.endVisitNode(node); }
    public endVisitEnumDefinition(node: EnumDefinition) { this.endVisitNode(node); }
    public endVisitEnumValue(node: EnumValue) { this.endVisitNode(node); }
    public endVisitElementaryTypeName(node: ElementaryTypeName) { this.endVisitNode(node); }
    public endVisitArrayTypeName(node: ArrayTypeName) { this.endVisitNode(node); }
    public endVisitMapping(node: Mapping) { this.endVisitNode(node); }
    public endVisitFunctionTypeName(node: FunctionTypeName) { this.endVisitNode(node); }
    public endVisitUserDefinedTypeName(node: UserDefinedTypeName) { this.endVisitNode(node); }

    /// Generic function called by default for each node, to be overridden by derived classes
    /// if behaviour unspecific to a node type is desired.
    protected visitNode(_node: ASTNode): boolean { return true; }

    /// Generic function called by default for each node, to be overridden by derived classes
    /// if behaviour unspecific to a node type is desired.
    protected endVisitNode(_node: ASTNode) { }
}
