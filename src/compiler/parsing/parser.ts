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
    ContractKind,
    ElementaryTypeName,
    ElementaryTypeNameExpression,
    EnumDefinition,
    EnumValue,
    EventDefinition,
    Expression,
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
    Location,
    Mapping,
    MemberAccess,
    ModifierDefinition,
    ModifierInvocation,
    NewExpression,
    ParameterList,
    PlaceholderStatement,
    PragmaDirective,
    PrimaryExpression,
    Return,
    SourceUnit,
    StateMutability,
    Statement,
    StructDefinition,
    Throw,
    TupleExpression,
    TypeName,
    UnaryOperation,
    UserDefinedTypeName,
    UsingForDirective,
    VariableDeclaration,
    VariableDeclarationStatement,
    Visibility,
    WhileStatement,
    stateMutabilityToString,
    visibilityToString
} from "../ast/ast";
import { Debug, clone, first, last } from "../core";
import { DiagnosticReporter } from "../interface/diagnosticReporter";
import { CharStream, Scanner } from "../parsing/scanner";
import { SourceLocation } from "../types";
import { Token } from "./token";

export class ParserBase {
    protected diagnosticReporter: DiagnosticReporter;
    protected scanner: Scanner;

    constructor(errorReporter: DiagnosticReporter) {
        this.diagnosticReporter = errorReporter;
        this.scanner = new Scanner();
    }

    public get sourceName() {
        return this.scanner.sourceName;
    }

    /// Start position of the current token
    public get position(): number {
        return this.scanner.currentLocation.start;
    }

    /// End position of the current token
    public get endPosition(): number {
        return this.scanner.currentLocation.end;
    }

    protected get currentToken(): Token.TokenName {
        return this.scanner.currentToken;
    }

    protected get currentLiteral(): string {
        return this.scanner.currentLiteral;
    }

    protected peekNextToken(): Token.TokenName {
        return this.scanner.peekNextToken();
    }

    protected advance(): Token.TokenName {
        return this.scanner.next();
    }

    protected expectToken(value: Token.TokenName) {
        const tok = this.scanner.currentToken;
        if (tok !== value) {
            if (Token.isReservedKeyword(tok)) {
                this.fatalParserError(
                    `Expected token ${Token.tokenToName(value)} got reserved keyword '${Token.tokenToName(tok)}'`);
            }
            else if (Token.isElementaryTypeName(tok)) { // for the sake of accuracy in reporting
                const elemTypeName = this.scanner.currentElementaryTypeNameToken;
                this.fatalParserError(
                    `Expected token ${Token.tokenToName(value)} got '${elemTypeName}'`);
            }
            else
                this.fatalParserError(
                    `Expected token ${Token.tokenToName(value)} got '${Token.tokenToName(this.scanner.currentToken)}'`);
        }
        this.scanner.next();
    }

    protected fatalParserError(description: string) {
        this.diagnosticReporter.fatalParserError(
            description, new SourceLocation(this.position, this.position, this.sourceName));
    }

    protected parserError(description: string) {
        this.diagnosticReporter.parserError(
            description, new SourceLocation(this.position, this.position, this.sourceName));
    }
}

/// This struct is shared for parsing a function header and a function type.
class FunctionHeaderParserResult {
    public name: string;
    public parameters: ParameterList;
    public returnParameters: ParameterList;
    public visibility = Visibility.Default;
    public stateMutability = StateMutability.NonPayable;
    public modifiers: ModifierInvocation[] = [];
}

class VarDeclParserOptions {
    public allowVar = false;
    public isStateVariable = false;
    public allowIndexed = false;
    public allowEmptyName = false;
    public allowInitialValue = false;
    public allowLocationSpecifier = false;
}

/// Used as return value of @see peekStatementType.
const enum LookAheadInfo {
    IndexAccessStructure, VariableDeclarationStatement, ExpressionStatement
}

export class Parser extends ParserBase {
    /// Flag that signifies whether '_' is parsed as a PlaceholderStatement or a regular identifier.
    private insideModifier = false;

    constructor(errorReporter: DiagnosticReporter) {
        super(errorReporter);
    }

    private get currentTokenName(): string {
        const token = this.scanner.currentToken;
        if (Token.isElementaryTypeName(token)) { // for the sake of accuracy in reporting
            const elemTypeName = this.scanner.currentElementaryTypeNameToken;
            return elemTypeName.toString();
        }
        else
            return Token.tokenToName(token);
    }

    private expectAssignmentOperator(): Token.TokenName {
        const op = this.scanner.currentToken;
        if (!Token.isAssignmentOp(op))
            this.fatalParserError(`Expected assignment operator,  got '${this.currentTokenName}'`);
        this.scanner.next();
        return op;
    }

    private expectIdentifierToken(): string {
        const id = this.scanner.currentToken;
        if (id !== Token.TokenName.Identifier)
            this.fatalParserError(`Expected identifier, got '${this.currentTokenName}'`);
        return this.getLiteralAndAdvance();
    }

    private getLiteralAndAdvance(): string {
        const identifier = this.scanner.currentLiteral;
        this.scanner.next();
        return identifier;
    }

    private createEmptyParameterList(): ParameterList {
        const nodeFactory = new ASTNodeFactory(this);
        nodeFactory.setLocationEmpty();
        return nodeFactory.createNode(ParameterList, []);
    }

    private typeNameIndexAccessStructure(
        path: PrimaryExpression[],
        indices: [Expression, SourceLocation][]
    ): TypeName {
        Debug.assert(path.length !== 0);
        const nodeFactory = new ASTNodeFactory(this);
        const location = first(path).location;
        location.end = last(path).location.end;
        nodeFactory.setLocation(location);

        let type: TypeName;
        const typeName = first(path) as ElementaryTypeNameExpression;
        if (type) {
            Debug.assert(path.length === 1);
            type = nodeFactory.createNode(ElementaryTypeName, typeName.typeName);
        }
        else {
            const namePath: string[] = [];
            for (const el of path)
                namePath.push((el as Identifier).name);
            type = nodeFactory.createNode(UserDefinedTypeName, namePath);
        }
        for (const [first, second] of indices) {
            nodeFactory.setLocation(second);
            type = nodeFactory.createNode(ArrayTypeName, type, first);
        }
        return type;
    }

    private expressionFromIndexAccessStructure(
        path: PrimaryExpression[],
        indices: [Expression, SourceLocation][]): Expression {
        Debug.assert(path.length !== 0);
        const nodeFactory = new ASTNodeFactory(this);
        let expression = first(path);
        for (let i = 1; i < path.length; ++i) {
            const location = first(path).location;
            location.end = path[i].location.end;
            nodeFactory.setLocation(location);
            const identifier = path[i] as Identifier;
            expression = nodeFactory.createNode(
                MemberAccess,
                expression,
                identifier.name);
        }
        for (const [first, second] of indices) {
            nodeFactory.setLocation(second);
            expression = nodeFactory.createNode(IndexAccess, expression, first);
        }
        return expression;
    }

    private peekStatementType(): LookAheadInfo {
        // Distinguish between variable declaration (and potentially assignment) and expression statement
        // (which include assignments to other expressions and pre-declared variables).
        // We have a variable declaration if we get a keyword that specifies a type name.
        // If it is an identifier or an elementary type name followed by an identifier, we also have
        // a variable declaration.
        // If we get an identifier followed by a "[" or ".", it can be both ("lib.type[9] a;" or "variable.el[9] = 7;").
        // In all other cases, we have an expression statement.
        const token = this.scanner.currentToken;
        const mightBeTypeName = (Token.isElementaryTypeName(token) || token === Token.TokenName.Identifier);

        if (token === Token.TokenName.Mapping || token === Token.TokenName.Function || token === Token.TokenName.Var)
            return LookAheadInfo.VariableDeclarationStatement;
        if (mightBeTypeName) {
            const next = this.scanner.peekNextToken();
            if (next === Token.TokenName.Identifier || Token.isLocationSpecifier(next))
                return LookAheadInfo.VariableDeclarationStatement;
            if (next === Token.TokenName.LBrack || next === Token.TokenName.Period)
                return LookAheadInfo.IndexAccessStructure;
        }
        return LookAheadInfo.ExpressionStatement;
    }

    public parse(scanner: Scanner, sourceText = ""): SourceUnit {
        const withRecursionGuard = <T>(f: () => T): T => {
            try {
                return f();
            } catch (err) {
                if (err.message === "Maximum call stack size exceeded") {
                    this.fatalParserError("Maximum recursion depth reached during parsing.");
                }
                throw err;
            }
        };

        try {
            return withRecursionGuard(() => {
                this.scanner = scanner;
                const nodeFactory = new ASTNodeFactory(this);
                const nodes: ASTNode[] = [];
                while (this.scanner.currentToken !== Token.TokenName.EOS) {
                    const token = this.scanner.currentToken;
                    switch (token) {
                        case Token.TokenName.Pragma:
                            nodes.push(this.parsePragmaDirective());
                            break;
                        case Token.TokenName.Import:
                            nodes.push(this.parseImportDirective());
                            break;
                        case Token.TokenName.Interface:
                        case Token.TokenName.Contract:
                        case Token.TokenName.Library:
                            nodes.push(this.parseContractDefinition(token));
                            break;
                        default:
                            this.fatalParserError("Expected pragma, import directive or contract/interface/library definition.");
                    }
                }
                return nodeFactory.createNode(SourceUnit, sourceText, nodes);
            });
        } catch (err) {
            if (this.diagnosticReporter.diagnostics.length === 0)
                throw new Error("Something went wrong"); // Something is weird here, rather throw again.
            return undefined;
        }
    }

    public parseSourceFile(fileName: string, sourceText: string): SourceUnit {
        const scanner = new Scanner(new CharStream(sourceText), fileName);
        return this.parse(scanner, sourceText);
    }

    private parsePragmaDirective() {
        // pragma anything* ;
        // Currently supported:
        // pragma solidity ^0.4.0 || ^0.3.0;
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.Pragma);
        const literals: string[] = [];
        const tokens: Token.TokenName[] = [];
        do {
            const token = this.scanner.currentToken;
            if (token === Token.TokenName.Illegal)
                this.parserError("Token incompatible with Solidity parser as part of pragma directive.");
            else {
                let literal = this.scanner.currentLiteral;
                if (literal === "" && Token.tokenToString(token))
                    literal = Token.tokenToString(token);
                literals.push(literal);
                tokens.push(token);
            }
            this.scanner.next();
        }
        while (this.scanner.currentToken !== Token.TokenName.Semicolon && this.scanner.currentToken !== Token.TokenName.EOS);
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.Semicolon);
        return nodeFactory.createNode(PragmaDirective, tokens, literals);
    }

    private parseImportDirective(): ImportDirective {
        // import "abc" [as x];
        // import * as x from "abc";
        // import {a as b, c} from "abc";
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.Import);
        let path;
        let unitAlias = "";
        const symbolAliases: [Identifier, string][] = [];

        if (this.scanner.currentToken === Token.TokenName.StringLiteral) {
            path = this.getLiteralAndAdvance();
            if (this.scanner.currentToken as Token.TokenName === Token.TokenName.As) {
                this.scanner.next();
                unitAlias = this.expectIdentifierToken();
            }
        }
        else {
            if (this.scanner.currentToken === Token.TokenName.LBrace) {
                this.scanner.next();
                while (true) {
                    const id = this.parseIdentifier();
                    let alias;
                    if (this.scanner.currentToken as Token.TokenName === Token.TokenName.As) {
                        this.expectToken(Token.TokenName.As);
                        alias = this.expectIdentifierToken();
                    }
                    symbolAliases.push([id, alias]);
                    if (this.scanner.currentToken as Token.TokenName !== Token.TokenName.Comma)
                        break;
                    this.scanner.next();
                }
                this.expectToken(Token.TokenName.RBrace);
            }
            else if (this.scanner.currentToken === Token.TokenName.Mul) {
                this.scanner.next();
                this.expectToken(Token.TokenName.As);
                unitAlias = this.expectIdentifierToken();
            }
            else
                this.fatalParserError("Expected string literal (path), \"*\" or alias list.");
            // "from" is not a keyword but parsed as an identifier because of backwards
            // compatibility and because it is a really common word.
            if (this.scanner.currentToken !== Token.TokenName.Identifier || this.scanner.currentLiteral !== "from")
                this.fatalParserError("Expected \"from\".");
            this.scanner.next();
            if (this.scanner.currentToken as Token.TokenName !== Token.TokenName.StringLiteral)
                this.fatalParserError("Expected import path.");
            path = this.getLiteralAndAdvance();
        }
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.Semicolon);
        return nodeFactory.createNode(ImportDirective, path, unitAlias, symbolAliases);
    }

    private tokenToContractKind(token: Token.TokenName): ContractKind {
        switch (token) {
            case Token.TokenName.Interface:
                return ContractKind.Interface;
            case Token.TokenName.Contract:
                return ContractKind.Contract;
            case Token.TokenName.Library:
                return ContractKind.Library;
            default:
                this.fatalParserError("Unsupported contract type.");
        }
        // FIXME: fatalParserError is not considered as throwing here
        return ContractKind.Contract;
    }

    private parseContractDefinition(expectedKind: Token.TokenName): ContractDefinition {
        const nodeFactory = new ASTNodeFactory(this);
        let docString: string;
        if (this.scanner.currentCommentLiteral !== "")
            docString = this.scanner.currentCommentLiteral;
        this.expectToken(expectedKind);
        const name = this.expectIdentifierToken();
        const baseContracts: InheritanceSpecifier[] = [];
        if (this.scanner.currentToken === Token.TokenName.Is)
            do {
                this.scanner.next();
                baseContracts.push(this.parseInheritanceSpecifier());
            } while (this.scanner.currentToken as Token.TokenName === Token.TokenName.Comma);
        const subNodes: ASTNode[] = [];
        this.expectToken(Token.TokenName.LBrace);
        while (true) {
            const currentTokenValue = this.scanner.currentToken;
            if (currentTokenValue === Token.TokenName.RBrace)
                break;
            else if (currentTokenValue === Token.TokenName.Function)
                // This can be a function or a state variable of function type (especially
                // complicated to distinguish fallback function from function type state variable)
                subNodes.push(this.parseFunctionDefinitionOrFunctionTypeStateVariable(name));
            else if (currentTokenValue === Token.TokenName.Struct)
                subNodes.push(this.parseStructDefinition());
            else if (currentTokenValue === Token.TokenName.Enum)
                subNodes.push(this.parseEnumDefinition());
            else if (
                currentTokenValue === Token.TokenName.Identifier ||
                currentTokenValue === Token.TokenName.Mapping ||
                Token.isElementaryTypeName(currentTokenValue)) {
                const options = new VarDeclParserOptions();
                options.isStateVariable = true;
                options.allowInitialValue = true;
                subNodes.push(this.parseVariableDeclaration(options));
                this.expectToken(Token.TokenName.Semicolon);
            }
            else if (currentTokenValue === Token.TokenName.Modifier)
                subNodes.push(this.parseModifierDefinition());
            else if (currentTokenValue === Token.TokenName.Event)
                subNodes.push(this.parseEventDefinition());
            else if (currentTokenValue === Token.TokenName.Using)
                subNodes.push(this.parseUsingDirective());
            else
                this.fatalParserError("Function, variable, struct or modifier declaration expected.");
        }
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.RBrace);
        return nodeFactory.createNode(
            ContractDefinition,
            name,
            docString,
            baseContracts,
            subNodes,
            this.tokenToContractKind(expectedKind));
    }

    private parseInheritanceSpecifier(): InheritanceSpecifier {
        const nodeFactory = new ASTNodeFactory(this);
        const name = this.parseUserDefinedTypeName();
        let args: Expression[] = [];
        if (this.scanner.currentToken === Token.TokenName.LParen) {
            this.scanner.next();
            args = this.parseFunctionCallListArguments();
            nodeFactory.markEndPosition();
            this.expectToken(Token.TokenName.RParen);
        }
        else
            nodeFactory.setEndPositionFromNode(name);
        return nodeFactory.createNode(InheritanceSpecifier, name, arguments);
    }

    private parseBlock(docString = ""): Block {
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.LBrace);
        const statements: Statement[] = [];
        while (this.scanner.currentToken !== Token.TokenName.RBrace)
            statements.push(this.parseStatement());
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.RBrace);
        return nodeFactory.createNode(Block, docString, statements);
    }

    private parseStatement(): Statement {
        let docString: string;
        if (this.scanner.currentCommentLiteral !== "")
            docString = this.scanner.currentCommentLiteral;
        let statement: Statement;
        switch (this.scanner.currentToken) {
            case Token.TokenName.If:
                return this.parseIfStatement(docString);
            case Token.TokenName.While:
                return this.parseWhileStatement(docString);
            case Token.TokenName.Do:
                return this.parseDoWhileStatement(docString);
            case Token.TokenName.For:
                return this.parseForStatement(docString);
            case Token.TokenName.LBrace:
                return this.parseBlock(docString);
            // starting from here, all statements must be terminated by a semicolon
            case Token.TokenName.Continue:
                statement = new ASTNodeFactory(this).createNode(Continue, docString);
                this.scanner.next();
                break;
            case Token.TokenName.Break:
                statement = new ASTNodeFactory(this).createNode(Break, docString);
                this.scanner.next();
                break;
            case Token.TokenName.Return:
                {
                    const nodeFactory = new ASTNodeFactory(this);
                    let expression: Expression;
                    if (this.scanner.next() !== Token.TokenName.Semicolon) {
                        expression = this.parseExpression();
                        nodeFactory.setEndPositionFromNode(expression);
                    }
                    statement = nodeFactory.createNode(Return, docString, expression);
                    break;
                }
            case Token.TokenName.Throw:
                {
                    statement = new ASTNodeFactory(this).createNode(Throw, docString);
                    this.scanner.next();
                    break;
                }
            case Token.TokenName.Assembly:
                this.fatalParserError("Inline assembly is not supported yet.");
                break;
            case Token.TokenName.Identifier:
                if (this.insideModifier && this.scanner.currentLiteral === "_") {
                    statement = new ASTNodeFactory(this).createNode(PlaceholderStatement, docString);
                    this.scanner.next();
                }
                else
                    statement = this.parseSimpleStatement(docString);
                break;
            default:
                statement = this.parseSimpleStatement(docString);
                break;
        }
        this.expectToken(Token.TokenName.Semicolon);
        return statement;
    }

    private parseIfStatement(docString = ""): IfStatement {
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.If);
        this.expectToken(Token.TokenName.LParen);
        const condition = this.parseExpression();
        this.expectToken(Token.TokenName.RParen);
        const trueBody = this.parseStatement();
        let falseBody: Statement;
        if (this.scanner.currentToken === Token.TokenName.Else) {
            this.scanner.next();
            falseBody = this.parseStatement();
            nodeFactory.setEndPositionFromNode(falseBody);
        }
        else
            nodeFactory.setEndPositionFromNode(trueBody);
        return nodeFactory.createNode(IfStatement, docString, condition, trueBody, falseBody);
    }

    private parseWhileStatement(docString: string): WhileStatement {
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.While);
        this.expectToken(Token.TokenName.LParen);
        const condition = this.parseExpression();
        this.expectToken(Token.TokenName.RParen);
        const body = this.parseStatement();
        nodeFactory.setEndPositionFromNode(body);
        return nodeFactory.createNode(WhileStatement, docString, condition, body, false);
    }

    private parseDoWhileStatement(docString: string): WhileStatement {
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.Do);
        const body = this.parseStatement();
        this.expectToken(Token.TokenName.While);
        this.expectToken(Token.TokenName.LParen);
        const condition = this.parseExpression();
        this.expectToken(Token.TokenName.RParen);
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.Semicolon);
        return nodeFactory.createNode(WhileStatement, docString, condition, body, true);
    }

    private parseForStatement(docString: string): ForStatement {
        const nodeFactory = new ASTNodeFactory(this);
        let initExpression: Statement;
        let conditionExpression: Expression;
        let loopExpression: ExpressionStatement;
        this.expectToken(Token.TokenName.For);
        this.expectToken(Token.TokenName.LParen);

        // LTODO: Maybe here have some predicate like peekExpression() instead of checking for semicolon and RParen?
        if (this.scanner.currentToken !== Token.TokenName.Semicolon)
            initExpression = this.parseSimpleStatement();
        this.expectToken(Token.TokenName.Semicolon);

        if (this.scanner.currentToken !== Token.TokenName.Semicolon)
            conditionExpression = this.parseExpression();
        this.expectToken(Token.TokenName.Semicolon);

        if (this.scanner.currentToken !== Token.TokenName.RParen)
            loopExpression = this.parseExpressionStatement();
        this.expectToken(Token.TokenName.RParen);

        const body = this.parseStatement();
        nodeFactory.setEndPositionFromNode(body);
        return nodeFactory.createNode(
            ForStatement,
            docString,
            initExpression,
            conditionExpression,
            loopExpression,
            body);
    }

    private parseSimpleStatement(docString = ""): Statement {
        // These two cases are very hard to distinguish:
        // x[7 * 20 + 3] a;  -  x[7 * 20 + 3] = 9;
        // In the first case, x is a type name, in the second it is the name of a variable.
        // As an extension, we can even have:
        // `x.y.z[1][2] a;` and `x.y.z[1][2] = 10;`
        // Where in the first, x.y.z leads to a type name where in the second, it accesses structs.
        switch (this.peekStatementType()) {
            case LookAheadInfo.VariableDeclarationStatement:
                return this.parseVariableDeclarationStatement(docString);
            case LookAheadInfo.ExpressionStatement:
                return this.parseExpressionStatement(docString);
            default:
                break;
        }
        // At this point, we have 'Identifier "["' or 'Identifier "." Identifier' or 'ElementoryTypeName "["'.
        // We parse '(Identifier ("." Identifier)* |ElementaryTypeName) ( "[" Expression "]" )+'
        // until we can decide whether to hand this over to ExpressionStatement or create a
        // VariableDeclarationStatement out of it.
        const path: PrimaryExpression[] = [];
        let startedWithElementary = false;
        if (this.scanner.currentToken === Token.TokenName.Identifier)
            path.push(this.parseIdentifier());
        else {
            startedWithElementary = true;
            const { m, n } = this.scanner.currentTokenInfo;
            const elemToken = new Token.ElementaryTypeNameToken(this.scanner.currentToken, m, n);
            path.push(new ASTNodeFactory(this).createNode(ElementaryTypeNameExpression, elemToken));
            this.scanner.next();
        }
        while (!startedWithElementary && this.scanner.currentToken === Token.TokenName.Period) {
            this.scanner.next();
            path.push(this.parseIdentifier());
        }
        const indices: [Expression, SourceLocation][] = [];
        while (this.scanner.currentToken === Token.TokenName.LBrack) {
            this.expectToken(Token.TokenName.LBrack);
            let index: Expression;
            if (this.scanner.currentToken as Token.TokenName !== Token.TokenName.RBrack)
                index = this.parseExpression();
            const indexLocation = first(path).location;
            indexLocation.end = this.endPosition;
            indices.push([index, indexLocation]);
            this.expectToken(Token.TokenName.RBrack);
        }

        if (this.scanner.currentToken === Token.TokenName.Identifier
            || Token.isLocationSpecifier(this.scanner.currentToken))
            return this.parseVariableDeclarationStatement(docString, this.typeNameIndexAccessStructure(path, indices));
        else
            return this.parseExpressionStatement(docString, this.expressionFromIndexAccessStructure(path, indices));
    }

    private parseVariableDeclarationStatement(
        docString = "",
        lookAheadArrayType?: TypeName): VariableDeclarationStatement {
        const nodeFactory = new ASTNodeFactory(this);
        if (lookAheadArrayType)
            nodeFactory.setLocation(lookAheadArrayType.location);
        const variables: VariableDeclaration[] = [];
        let value: Expression;
        if (
            !lookAheadArrayType &&
            this.scanner.currentToken === Token.TokenName.Var &&
            this.scanner.peekNextToken() === Token.TokenName.LParen) {
            // Parse `var (a, b, ,, c) = ...` into a single VariableDeclarationStatement with multiple variables.
            this.scanner.next();
            this.scanner.next();
            if (this.scanner.currentToken as Token.TokenName !== Token.TokenName.RParen)
                while (true) {
                    let varDecl: VariableDeclaration;
                    if (
                        this.scanner.currentToken as Token.TokenName !== Token.TokenName.Comma &&
                        this.scanner.currentToken as Token.TokenName !== Token.TokenName.RParen) {
                        const varDeclNodeFactory = new ASTNodeFactory(this);
                        varDeclNodeFactory.markEndPosition();
                        const name = this.expectIdentifierToken();
                        varDecl = varDeclNodeFactory.createNode(
                            VariableDeclaration,
                            undefined,
                            name,
                            undefined,
                            Visibility.Default);
                    }
                    variables.push(varDecl);
                    if (this.scanner.currentToken as Token.TokenName === Token.TokenName.RParen)
                        break;
                    else
                        this.expectToken(Token.TokenName.Comma);
                }
            nodeFactory.markEndPosition();
            this.scanner.next();
        }
        else {
            const options = new VarDeclParserOptions();
            options.allowVar = true;
            options.allowLocationSpecifier = true;
            variables.push(this.parseVariableDeclaration(options, lookAheadArrayType));
            nodeFactory.setEndPositionFromNode(last(variables));
        }
        if (this.scanner.currentToken === Token.TokenName.Assign) {
            this.scanner.next();
            value = this.parseExpression();
            nodeFactory.setEndPositionFromNode(value);
        }
        return nodeFactory.createNode(VariableDeclarationStatement, docString, variables, value);
    }

    private parseExpressionStatement(
        docString = "",
        lookAheadIndexAccessStructure?: Expression): ExpressionStatement {
        const expression = this.parseExpression(lookAheadIndexAccessStructure);
        return new ASTNodeFactory(this, expression).createNode(ExpressionStatement, docString, expression);
    }

    private parseExpression(lookAheadIndexAccessStructure?: Expression): Expression {
        const expression = this.parseBinaryExpression(4, lookAheadIndexAccessStructure);
        if (Token.isAssignmentOp(this.scanner.currentToken)) {
            const assignmentOperator = this.expectAssignmentOperator();
            const rightHandSide = this.parseExpression();
            const nodeFactory = new ASTNodeFactory(this);
            nodeFactory.setEndPositionFromNode(rightHandSide);
            return nodeFactory.createNode(Assignment, expression, assignmentOperator, rightHandSide);
        }
        else if (this.scanner.currentToken === Token.TokenName.Conditional) {
            this.scanner.next();
            const trueExpression = this.parseExpression();
            this.expectToken(Token.TokenName.Colon);
            const falseExpression = this.parseExpression();
            const nodeFactory = new ASTNodeFactory(this);
            nodeFactory.setEndPositionFromNode(falseExpression);
            return nodeFactory.createNode(Conditional, expression, trueExpression, falseExpression);
        }
        else
            return expression;
    }

    private parseBinaryExpression(
        minPrecedence = 4,
        lookAheadIndexAccessStructure?: Expression
    ): Expression {
        let expression = this.parseUnaryExpression(lookAheadIndexAccessStructure);
        const nodeFactory = new ASTNodeFactory(this);
        let prec = Token.precedence(this.scanner.currentToken);
        for (; prec >= minPrecedence; --prec) {
            while (Token.precedence(this.scanner.currentToken) === prec) {
                const op = this.scanner.currentToken;
                this.scanner.next();
                const right = this.parseBinaryExpression(prec + 1);
                nodeFactory.setEndPositionFromNode(right);
                expression = nodeFactory.createNode(BinaryOperation, expression, op, right);
            }
        }
        return expression;
    }

    private parseUnaryExpression(lookAheadIndexAccessStructure?: Expression): Expression {
        const nodeFactory = lookAheadIndexAccessStructure ?
            new ASTNodeFactory(this, lookAheadIndexAccessStructure) : new ASTNodeFactory(this);

        let token = this.scanner.currentToken;
        if (!lookAheadIndexAccessStructure && (Token.isUnaryOp(token) || Token.isCountOp(token))) {
            // prefix expression
            this.scanner.next();
            const subExpression = this.parseUnaryExpression();
            nodeFactory.setEndPositionFromNode(subExpression);
            return nodeFactory.createNode(UnaryOperation, token, subExpression, true);
        }
        else {
            // potential postfix expression
            const subExpression = this.parseLeftHandSideExpression(lookAheadIndexAccessStructure);
            token = this.scanner.currentToken;
            if (!Token.isCountOp(token))
                return subExpression;
            nodeFactory.markEndPosition();
            this.scanner.next();
            return nodeFactory.createNode(UnaryOperation, token, subExpression, false);
        }
    }

    private parseLeftHandSideExpression(lookAheadIndexAccessStructure?: Expression): Expression {
        const nodeFactory = lookAheadIndexAccessStructure ?
            new ASTNodeFactory(this, lookAheadIndexAccessStructure) : new ASTNodeFactory(this);

        let expression: Expression;
        if (lookAheadIndexAccessStructure)
            expression = lookAheadIndexAccessStructure;
        else if (this.scanner.currentToken === Token.TokenName.New) {
            this.expectToken(Token.TokenName.New);
            const typeName = this.parseTypeName(false);
            if (typeName)
                nodeFactory.setEndPositionFromNode(typeName);
            else
                nodeFactory.markEndPosition();
            expression = nodeFactory.createNode(NewExpression, typeName);
        }
        else
            expression = this.parsePrimaryExpression();

        while (true) {
            switch (this.scanner.currentToken) {
                case Token.TokenName.LBrack:
                    {
                        this.scanner.next();
                        let index: Expression;
                        if (this.scanner.currentToken as Token.TokenName !== Token.TokenName.RBrack)
                            index = this.parseExpression();
                        nodeFactory.markEndPosition();
                        this.expectToken(Token.TokenName.RBrack);
                        expression = nodeFactory.createNode(IndexAccess, expression, index);
                        break;
                    }
                case Token.TokenName.Period:
                    {
                        this.scanner.next();
                        nodeFactory.markEndPosition();
                        expression = nodeFactory.createNode(MemberAccess, expression, this.expectIdentifierToken());
                        break;
                    }
                case Token.TokenName.LParen:
                    {
                        this.scanner.next();
                        const { args, names } = this.parseFunctionCallArguments();
                        nodeFactory.markEndPosition();
                        this.expectToken(Token.TokenName.RParen);
                        expression = nodeFactory.createNode(FunctionCall, expression, args, names);
                        break;
                    }
                default:
                    return expression;
            }
        }
    }

    private parseFunctionCallArguments(): { args: Expression[], names: string[] } {
        const ret: { args: Expression[], names: string[] } = { args: [], names: [] };
        const token = this.scanner.currentToken;
        if (token === Token.TokenName.LBrace) {
            // call({arg1 : 1, arg2 : 2 })
            this.expectToken(Token.TokenName.LBrace);

            let first = true;
            while (this.scanner.currentToken !== Token.TokenName.RBrace) {
                if (!first)
                    this.expectToken(Token.TokenName.Comma);

                ret.names.push(this.expectIdentifierToken());
                this.expectToken(Token.TokenName.Colon);
                ret.args.push(this.parseExpression());

                if (
                    this.scanner.currentToken === Token.TokenName.Comma &&
                    this.scanner.peekNextToken() === Token.TokenName.RBrace
                ) {
                    this.parserError("Unexpected trailing comma.");
                    this.scanner.next();
                }

                first = false;
            }
            this.expectToken(Token.TokenName.RBrace);
        }
        else
            ret.args = this.parseFunctionCallListArguments();
        return ret;
    }

    private parseFunctionCallListArguments(): Expression[] {
        const args: Expression[] = [];
        if (this.scanner.currentToken !== Token.TokenName.RParen) {
            args.push(this.parseExpression());
            while (this.scanner.currentToken as Token.TokenName !== Token.TokenName.RParen) {
                this.expectToken(Token.TokenName.Comma);
                args.push(this.parseExpression());
            }
        }
        return args;
    }

    private parsePrimaryExpression(): Expression {
        const nodeFactory = new ASTNodeFactory(this);
        const token = this.scanner.currentToken;
        let expression: Expression;

        switch (token) {
            case Token.TokenName.TrueLiteral:
            case Token.TokenName.FalseLiteral:
                nodeFactory.markEndPosition();
                expression = nodeFactory.createNode(Literal, token, this.getLiteralAndAdvance());
                break;
            case Token.TokenName.Number:
                if (Token.isEtherSubdenomination(this.scanner.peekNextToken())) {
                    const literal = this.getLiteralAndAdvance();
                    nodeFactory.markEndPosition();
                    const subdenomination = this.scanner.currentToken;
                    this.scanner.next();
                    expression = nodeFactory.createNode(Literal, token, literal, subdenomination);
                }
                else if (Token.isTimeSubdenomination(this.scanner.peekNextToken())) {
                    const literal = this.getLiteralAndAdvance();
                    nodeFactory.markEndPosition();
                    const subdenomination = this.scanner.currentToken;
                    this.scanner.next();
                    expression = nodeFactory.createNode(Literal, token, literal, subdenomination);
                }
                else {
                    nodeFactory.markEndPosition();
                    expression = nodeFactory.createNode(Literal, token, this.getLiteralAndAdvance());
                }
                break;
            case Token.TokenName.StringLiteral:
                nodeFactory.markEndPosition();
                expression = nodeFactory.createNode(Literal, token, this.getLiteralAndAdvance());
                break;
            case Token.TokenName.Identifier:
                nodeFactory.markEndPosition();
                expression = nodeFactory.createNode(Identifier, this.getLiteralAndAdvance());
                break;
            case Token.TokenName.LParen:
            case Token.TokenName.LBrack:
                {
                    // Tuple/parenthesized expression or inline array/bracketed expression.
                    // Special cases: ()/[] is empty tuple/array type, (x) is not a real tuple,
                    // (x,) is one-dimensional tuple, elements in arrays cannot be left out, only in tuples.
                    this.scanner.next();
                    const components: Expression[] = [];
                    const oppositeToken = (token === Token.TokenName.LParen ? Token.TokenName.RParen : Token.TokenName.RBrack);
                    const isArray = (token === Token.TokenName.LBrack);

                    if (this.scanner.currentToken !== oppositeToken)
                        while (true) {
                            if (this.scanner.currentToken !== Token.TokenName.Comma && this.scanner.currentToken !== oppositeToken)
                                components.push(this.parseExpression());
                            else if (isArray)
                                this.parserError("Expected expression (inline array elements cannot be omitted).");
                            else
                                components.push(undefined);

                            if (this.scanner.currentToken === oppositeToken)
                                break;

                            this.expectToken(Token.TokenName.Comma);
                        }
                    nodeFactory.markEndPosition();
                    this.expectToken(oppositeToken);
                    expression = nodeFactory.createNode(TupleExpression, components, isArray);
                    break;
                }
            default:
                if (Token.isElementaryTypeName(token)) {
                    // used for casts
                    const { m, n } = this.scanner.currentTokenInfo;
                    const elementaryExpression = new Token.ElementaryTypeNameToken(this.scanner.currentToken, m, n);
                    expression = nodeFactory.createNode(ElementaryTypeNameExpression, elementaryExpression);
                    this.scanner.next();
                }
                else
                    this.fatalParserError("Expected primary expression.");
                break;
        }
        return expression;
    }

    private parseIdentifier(): Identifier {
        const nodeFactory = new ASTNodeFactory(this);
        nodeFactory.markEndPosition();
        return nodeFactory.createNode(Identifier, this.expectIdentifierToken());
    }

    private parseEventDefinition(): EventDefinition {
        const nodeFactory = new ASTNodeFactory(this);
        let docstring: string;
        if (this.scanner.currentCommentLiteral !== "")
            docstring = this.scanner.currentCommentLiteral;

        this.expectToken(Token.TokenName.Event);
        const name = this.expectIdentifierToken();
        let parameters: ParameterList;
        if (this.scanner.currentToken === Token.TokenName.LParen) {
            const options = new VarDeclParserOptions();
            options.allowIndexed = true;
            parameters = this.parseParameterList(options);
        }
        else
            parameters = this.createEmptyParameterList();
        let anonymous = false;
        if (this.scanner.currentToken === Token.TokenName.Anonymous) {
            anonymous = true;
            this.scanner.next();
        }
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.Semicolon);
        return nodeFactory.createNode(EventDefinition, name, docstring, parameters, anonymous);
    }

    private parseUsingDirective(): UsingForDirective {
        const nodeFactory = new ASTNodeFactory(this);

        this.expectToken(Token.TokenName.Using);
        const library = this.parseUserDefinedTypeName();
        let typeName: TypeName;
        this.expectToken(Token.TokenName.For);
        if (this.scanner.currentToken === Token.TokenName.Mul)
            this.scanner.next();
        else
            typeName = this.parseTypeName(false);
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.Semicolon);
        return nodeFactory.createNode(UsingForDirective, library, typeName);
    }

    private parseFunctionDefinitionOrFunctionTypeStateVariable(contractName: string): ASTNode {
        const nodeFactory = new ASTNodeFactory(this);
        let docstring: string;
        if (this.scanner.currentCommentLiteral !== "")
            docstring = this.scanner.currentCommentLiteral;

        const header = this.parseFunctionHeader(false, true);

        if (
            header.modifiers.length !== 0 ||
            header.name !== "" ||
            this.scanner.currentToken === Token.TokenName.Semicolon ||
            this.scanner.currentToken === Token.TokenName.LBrace) {
            // this has to be a function
            let block: Block;
            nodeFactory.markEndPosition();
            if (this.scanner.currentToken !== Token.TokenName.Semicolon) {
                block = this.parseBlock();
                nodeFactory.setEndPositionFromNode(block);
            }
            else
                this.scanner.next(); // just consume the ';'
            const c_isConstructor = (contractName && header.name === contractName);
            return nodeFactory.createNode(
                FunctionDefinition,
                header.name,
                header.visibility,
                header.stateMutability,
                c_isConstructor,
                docstring,
                header.parameters,
                header.modifiers,
                header.returnParameters,
                block);
        }
        else {
            // this has to be a state variable
            let type: TypeName = nodeFactory.createNode(
                FunctionTypeName,
                header.parameters,
                header.returnParameters,
                header.visibility,
                header.stateMutability);
            type = this.parseTypeNameSuffix(type, nodeFactory);
            const options = new VarDeclParserOptions();
            options.isStateVariable = true;
            options.allowInitialValue = true;
            const node = this.parseVariableDeclaration(options, type);
            this.expectToken(Token.TokenName.Semicolon);
            return node;
        }
    }

    private parseStructDefinition(): StructDefinition {
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.Struct);
        const name = this.expectIdentifierToken();
        const members: VariableDeclaration[] = [];
        this.expectToken(Token.TokenName.LBrace);
        while (this.scanner.currentToken !== Token.TokenName.RBrace) {
            members.push(this.parseVariableDeclaration());
            this.expectToken(Token.TokenName.Semicolon);
        }
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.RBrace);
        return nodeFactory.createNode(StructDefinition, name, members);
    }

    private parseModifierDefinition(): ModifierDefinition {
        try {
            this.insideModifier = true;
            const nodeFactory = new ASTNodeFactory(this);
            let docstring: string;
            if (this.scanner.currentCommentLiteral !== "")
                docstring = this.scanner.currentCommentLiteral;

            this.expectToken(Token.TokenName.Modifier);
            const name = this.expectIdentifierToken();
            let parameters: ParameterList;
            if (this.scanner.currentToken === Token.TokenName.LParen) {
                const options = new VarDeclParserOptions();
                options.allowIndexed = true;
                options.allowLocationSpecifier = true;
                parameters = this.parseParameterList(options);
            }
            else
                parameters = this.createEmptyParameterList();
            const block = this.parseBlock();
            nodeFactory.setEndPositionFromNode(block);
            return nodeFactory.createNode(ModifierDefinition, name, docstring, parameters, block);

        } finally {
            this.insideModifier = false;
        }
    }

    private parseEnumDefinition(): EnumDefinition {
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.Enum);
        const name = this.expectIdentifierToken();
        const members: EnumValue[] = [];
        this.expectToken(Token.TokenName.LBrace);

        while (this.scanner.currentToken !== Token.TokenName.RBrace) {
            members.push(this.parseEnumValue());
            if (this.scanner.currentToken as Token.TokenName === Token.TokenName.RBrace)
                break;
            this.expectToken(Token.TokenName.Comma);
            if (this.scanner.currentToken !== Token.TokenName.Identifier)
                this.fatalParserError("Expected Identifier after ','");
        }
        if (members.length === 0)
            this.parserError("enum with no members is not allowed.");

        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.RBrace);
        return nodeFactory.createNode(EnumDefinition, name, members);
    }

    private parseEnumValue(): EnumValue {
        const nodeFactory = new ASTNodeFactory(this);
        nodeFactory.markEndPosition();
        return nodeFactory.createNode(EnumValue, this.expectIdentifierToken());
    }

    private parseMapping(): Mapping {
        const nodeFactory = new ASTNodeFactory(this);
        this.expectToken(Token.TokenName.Mapping);
        this.expectToken(Token.TokenName.LParen);
        let keyType: ElementaryTypeName;
        const token = this.scanner.currentToken;
        if (!Token.isElementaryTypeName(token))
            this.fatalParserError("Expected elementary type name for mapping key type");
        const { m, n } = this.scanner.currentTokenInfo;
        const elemTypeName = new Token.ElementaryTypeNameToken(token, m, n);
        keyType = new ASTNodeFactory(this).createNode(ElementaryTypeName, elemTypeName);
        this.scanner.next();
        this.expectToken(Token.TokenName.Arrow);
        const allowVar = false;
        const valueType = this.parseTypeName(allowVar);
        nodeFactory.markEndPosition();
        this.expectToken(Token.TokenName.RParen);
        return nodeFactory.createNode(Mapping, keyType, valueType);
    }

    private parseTypeName(allowVar: boolean): TypeName {
        const nodeFactory = new ASTNodeFactory(this);
        let type: TypeName;
        const token = this.scanner.currentToken;
        if (Token.isElementaryTypeName(token)) {
            const { m, n } = this.scanner.currentTokenInfo;
            const elemTypeName = new Token.ElementaryTypeNameToken(token, m, n);
            type = new ASTNodeFactory(this).createNode(ElementaryTypeName, elemTypeName);
            this.scanner.next();
        }
        else if (token === Token.TokenName.Var) {
            if (!allowVar)
                this.parserError("Expected explicit type name.");
            this.scanner.next();
        }
        else if (token === Token.TokenName.Function)
            type = this.parseFunctionType();
        else if (token === Token.TokenName.Mapping)
            type = this.parseMapping();
        else if (token === Token.TokenName.Identifier)
            type = this.parseUserDefinedTypeName();
        else
            this.fatalParserError("Expected type name");

        if (type)
            // Parse "[...]" postfixes for arrays.
            type = this.parseTypeNameSuffix(type, nodeFactory);
        return type;
    }

    private parseFunctionType(): FunctionTypeName {
        const nodeFactory = new ASTNodeFactory(this);
        const header = this.parseFunctionHeader(true, false);
        return nodeFactory.createNode(
            FunctionTypeName,
            header.parameters,
            header.returnParameters,
            header.visibility,
            header.stateMutability);
    }

    private parseFunctionHeader(forceEmptyName: boolean, allowModifiers: boolean): FunctionHeaderParserResult {
        const result = new FunctionHeaderParserResult();
        this.expectToken(Token.TokenName.Function);
        if (forceEmptyName || this.scanner.currentToken === Token.TokenName.LParen)
            result.name = "";
        else
            result.name = this.expectIdentifierToken();
        const options = new VarDeclParserOptions();
        options.allowLocationSpecifier = true;
        result.parameters = this.parseParameterList(options);
        while (true) {
            const token = this.scanner.currentToken;
            if (allowModifiers && token === Token.TokenName.Identifier) {
                // This can either be a modifier (function declaration) or the name of the
                // variable (function type name plus variable).
                if (
                    this.scanner.peekNextToken() === Token.TokenName.Semicolon ||
                    this.scanner.peekNextToken() === Token.TokenName.Assign)
                    // Variable declaration, break here.
                    break;
                else
                    result.modifiers.push(this.parseModifierInvocation());
            }
            else if (Token.isVisibilitySpecifier(token)) {
                if (result.visibility !== Visibility.Default) {
                    this.parserError(
                        `Visibility already specified as "${visibilityToString(result.visibility)}".`);
                    this.scanner.next();
                }
                else
                    result.visibility = this.parseVisibilitySpecifier(token);
            }
            else if (Token.isStateMutabilitySpecifier(token)) {
                if (result.stateMutability !== StateMutability.NonPayable) {
                    this.parserError(
                        `State mutability already specified as "${stateMutabilityToString(result.stateMutability)}".`);
                    this.scanner.next();
                }
                else
                    result.stateMutability = this.parseStateMutability(token);
            }
            else
                break;
        }
        if (this.scanner.currentToken === Token.TokenName.Returns) {
            const permitEmptyParameterList = false;
            this.scanner.next();
            result.returnParameters = this.parseParameterList(options, permitEmptyParameterList);
        }
        else
            result.returnParameters = this.createEmptyParameterList();
        return result;
    }

    private parseModifierInvocation(): ModifierInvocation {
        const nodeFactory = new ASTNodeFactory(this);
        const name = this.parseIdentifier();
        let args: Expression[] = [];
        if (this.scanner.currentToken === Token.TokenName.LParen) {
            this.scanner.next();
            args = this.parseFunctionCallListArguments();
            nodeFactory.markEndPosition();
            this.expectToken(Token.TokenName.RParen);
        }
        else
            nodeFactory.setEndPositionFromNode(name);
        return nodeFactory.createNode(ModifierInvocation, name, args);
    }

    private parseVisibilitySpecifier(token: Token.TokenName): Visibility {
        let visibility = Visibility.Default;
        if (token === Token.TokenName.Public)
            visibility = Visibility.Public;
        else if (token === Token.TokenName.Internal)
            visibility = Visibility.Internal;
        else if (token === Token.TokenName.Private)
            visibility = Visibility.Private;
        else if (token === Token.TokenName.External)
            visibility = Visibility.External;
        else
            Debug.assert(false, "Invalid visibility specifier.");
        this.scanner.next();
        return visibility;
    }

    private parseStateMutability(token: Token.TokenName): StateMutability {
        let stateMutability = StateMutability.NonPayable;
        if (token === Token.TokenName.Payable)
            stateMutability = StateMutability.Payable;
        // FIXME: constant should be removed at the next breaking release
        else if (token === Token.TokenName.View || token === Token.TokenName.Constant)
            stateMutability = StateMutability.View;
        else if (token === Token.TokenName.Pure)
            stateMutability = StateMutability.Pure;
        else
            Debug.assert(false, "Invalid state mutability specifier.");
        this.scanner.next();
        return stateMutability;
    }

    private parseTypeNameSuffix(type: TypeName, nodeFactory: ASTNodeFactory): TypeName {
        while (this.scanner.currentToken === Token.TokenName.LBrack) {
            this.scanner.next();
            let length: Expression;
            if (this.scanner.currentToken as Token.TokenName !== Token.TokenName.RBrack)
                length = this.parseExpression();
            nodeFactory.markEndPosition();
            this.expectToken(Token.TokenName.RBrack);
            type = nodeFactory.createNode(ArrayTypeName, type, length);
        }
        return type;
    }

    private parseUserDefinedTypeName(): UserDefinedTypeName {
        const nodeFactory = new ASTNodeFactory(this);
        nodeFactory.markEndPosition();
        const identifierPath = [this.expectIdentifierToken()];
        while (this.scanner.currentToken === Token.TokenName.Period) {
            this.scanner.next();
            nodeFactory.markEndPosition();
            identifierPath.push(this.expectIdentifierToken());
        }
        return nodeFactory.createNode(UserDefinedTypeName, identifierPath);
    }

    private parseParameterList(
        _options: VarDeclParserOptions,
        allowEmpty = true): ParameterList {
        const nodeFactory = new ASTNodeFactory(this);
        const parameters: VariableDeclaration[] = [];
        const options = clone(_options);
        options.allowEmptyName = true;
        this.expectToken(Token.TokenName.LParen);
        if (!allowEmpty || this.scanner.currentToken !== Token.TokenName.RParen) {
            parameters.push(this.parseVariableDeclaration(options));
            while (this.scanner.currentToken !== Token.TokenName.RParen) {
                if (this.scanner.currentToken === Token.TokenName.Comma && this.scanner.peekNextToken() === Token.TokenName.RParen)
                    this.fatalParserError("Unexpected trailing comma in parameter list.");
                this.expectToken(Token.TokenName.Comma);
                parameters.push(this.parseVariableDeclaration(options));
            }
        }
        nodeFactory.markEndPosition();
        this.scanner.next();
        return nodeFactory.createNode(ParameterList, parameters);
    }

    private parseVariableDeclaration(
        options = new VarDeclParserOptions(),
        lookAheadArrayType?: TypeName): VariableDeclaration {
        const nodeFactory = lookAheadArrayType ?
            new ASTNodeFactory(this, lookAheadArrayType) : new ASTNodeFactory(this);
        let type: TypeName;
        if (lookAheadArrayType)
            type = lookAheadArrayType;
        else {
            type = this.parseTypeName(options.allowVar);
            if (type)
                nodeFactory.setEndPositionFromNode(type);
        }
        let isIndexed = false;
        let isDeclaredConst = false;
        let visibility = Visibility.Default;
        let location = Location.Default;
        let identifier: string;

        while (true) {
            const token = this.scanner.currentToken;
            if (options.isStateVariable && Token.isVariableVisibilitySpecifier(token)) {
                if (visibility !== Visibility.Default) {
                    this.parserError(
                        `Visibility already specified as "${visibilityToString(visibility)}".`);
                    this.scanner.next();
                }
                else
                    visibility = this.parseVisibilitySpecifier(token);
            }
            else {
                if (options.allowIndexed && token === Token.TokenName.Indexed)
                    isIndexed = true;
                else if (token === Token.TokenName.Constant)
                    isDeclaredConst = true;
                else if (options.allowLocationSpecifier && Token.isLocationSpecifier(token)) {
                    if (location !== Location.Default)
                        this.parserError("Location already specified.");
                    else if (!type)
                        this.parserError("Location specifier needs explicit type name.");
                    else
                        location = (token === Token.TokenName.Memory ? Location.Memory : Location.Storage);
                }
                else
                    break;
                this.scanner.next();
            }
        }
        nodeFactory.markEndPosition();

        if (options.allowEmptyName && this.scanner.currentToken !== Token.TokenName.Identifier) {
            identifier = "";
            Debug.assert(type !== undefined);
            nodeFactory.setEndPositionFromNode(type);
        }
        else
            identifier = this.expectIdentifierToken();
        let value: Expression;
        if (options.allowInitialValue) {
            if (this.scanner.currentToken === Token.TokenName.Assign) {
                this.scanner.next();
                value = this.parseExpression();
                nodeFactory.setEndPositionFromNode(value);
            }
        }
        return nodeFactory.createNode(
            VariableDeclaration,
            type,
            identifier,
            value,
            visibility,
            options.isStateVariable,
            isIndexed,
            isDeclaredConst,
            location);
    }
}

class ASTNodeFactory {
    private location: SourceLocation;

    constructor(private parser: Parser, childNode?: ASTNode) {
        if (childNode) {
            this.location = clone(childNode.location);
        } else {
            this.location = new SourceLocation(parser.position, -1, parser.sourceName);
        }
    }

    public markEndPosition() {
        this.location.end = this.parser.endPosition;
    }

    public setEndPositionFromNode(node: ASTNode) {
        this.location.end = node.location.end;
    }

    public setLocation(location: SourceLocation) {
        this.location = clone(location);
    }

    public setLocationEmpty() {
        this.location.end = this.location.start;
    }

    public createNode<T>(cons: new (location: SourceLocation, ...args: any[]) => T, ...args: any[]): T {
        if (this.location.end < 0)
            this.markEndPosition();
        return new cons(this.location, ...args);
    }
}
