import {
    ASTNode,
    ContractDefinition,
    Declaration,
    EnumDefinition,
    EnumValue,
    EventDefinition,
    FunctionDefinition,
    Identifier,
    ImportDirective,
    MagicVariableDeclaration,
    ModifierDefinition,
    SourceUnit,
    StructDefinition,
    VariableDeclaration,
    VariableDeclarationStatement,
    VariableScope
} from "../ast/ast";
import { ASTVisitor } from "../ast/astVisitor";
import { Debug, findIndex, first, last, removeWhere } from "../core";
import { DiagnosticReporter } from "../interface/diagnosticReporter";
import { SecondarySourceLocation, SourceLocation, Map as SymbolMap } from "../types";
import { DeclarationContainer } from "./declarationContainer";
import { ReferencesResolver } from "./referenceResolver";

class DeclarationRegistrationHelper extends ASTVisitor {
    private currentFunction?: VariableScope;

    public static registerDeclaration(
        container: DeclarationContainer,
        declaration: Declaration,
        name: string,
        errorLocation: SourceLocation | undefined,
        warnOnShadow: boolean,
        diagnosticReporter: DiagnosticReporter
    ): boolean {
        if (!errorLocation)
            errorLocation = declaration.location;

        let shadowedDeclaration;
        if (warnOnShadow && declaration.name !== "" && container.enclosingContainer) {
            for (const decl of container.enclosingContainer.resolveName(declaration.name, true))
                shadowedDeclaration = decl;
        }

        if (!container.registerDeclaration(declaration, name, !declaration.isVisibleInContract())) {
            let firstDeclarationLocation: SourceLocation;
            let secondDeclarationLocation: SourceLocation;
            const conflictingDeclaration = container.conflictingDeclaration(declaration, name);
            Debug.assert(!!conflictingDeclaration);
            const comparable = errorLocation.sourceName &&
                conflictingDeclaration.location.sourceName &&
                errorLocation.sourceName === conflictingDeclaration.location.sourceName;
            if (comparable && errorLocation.start < conflictingDeclaration.location.start) {
                firstDeclarationLocation = errorLocation;
                secondDeclarationLocation = conflictingDeclaration.location;
            }
            else {
                firstDeclarationLocation = conflictingDeclaration.location;
                secondDeclarationLocation = errorLocation;
            }

            diagnosticReporter.declarationError(
                "Identifier already declared.",
                secondDeclarationLocation,
                new SecondarySourceLocation().append(firstDeclarationLocation, "The previous declaration is here:")
            );
            return false;
        }
        else if (shadowedDeclaration) {
            if (shadowedDeclaration instanceof MagicVariableDeclaration)
                diagnosticReporter.warning(
                    "This declaration shadows a builtin symbol.", declaration.location);
            else {
                const shadowedLocation = shadowedDeclaration.location;
                diagnosticReporter.warning(
                    "This declaration shadows an existing declaration.",
                    declaration.location,
                    new SecondarySourceLocation().append(shadowedLocation, "The shadowed declaration is here:")
                );
            }
        }
        return true;
    }

    /// Registers declarations in their scopes and creates new scopes as a side-effect
    /// of construction.
    /// @param _currentScope should be nullptr if we start at SourceUnit, but can be different
    /// to inject new declarations into an existing scope, used by snippets.
    constructor(
        private readonly scopes: Map<ASTNode, DeclarationContainer>,
        astRoot: ASTNode,
        private readonly diagnosticReporter: DiagnosticReporter,
        private currentScope?: ASTNode) {
        super();
        astRoot.accept(this);
        Debug.assert(this.currentScope === currentScope, "Scopes not correctly closed.");
    }

    private registerDeclaration(declaration: Declaration, opensScope: boolean) {
        Debug.assert(this.currentScope && this.scopes.has(this.currentScope), "No current scope.");

        let warnAboutShadowing = true;
        // Do not warn about shadowing for structs and enums because their members are
        // not accessible without prefixes. Also do not warn about event parameters
        // because they don't participate in any proper scope.
        if (this.currentScope instanceof StructDefinition ||
            this.currentScope instanceof EnumDefinition ||
            this.currentScope instanceof EventDefinition)
            warnAboutShadowing = false;
        // Do not warn about the constructor shadowing the contract.
        if (declaration instanceof FunctionDefinition) {
            if (declaration.isConstructor())
                warnAboutShadowing = false;
        }

        DeclarationRegistrationHelper.registerDeclaration(this.scopes.get(this.currentScope), declaration, "", undefined, warnAboutShadowing, this.diagnosticReporter);

        declaration.setScope(this.currentScope);
        if (opensScope)
            this.enterNewSubScope(declaration);
    }

    private get currentCanonicalName(): string {
        let ret: string = "";
        for (let scope = this.currentScope; !!scope; scope = this.scopes.get(scope).enclosingNode) {
            if (scope instanceof Declaration) {
                if (ret !== "")
                    ret = "." + ret;
                ret = scope.name + ret;
            }
        }
        return ret;
    }

    private enterNewSubScope(declaration: Declaration) {
        const container = new DeclarationContainer(this.currentScope, this.scopes.get(this.currentScope));
        Debug.assert(!this.scopes.has(declaration), "Unable to add new scope.");
        this.scopes.set(declaration, container);
        this.currentScope = declaration;
    }

    private closeCurrentScope() {
        Debug.assert(!!(this.currentScope && this.scopes.get(this.currentScope)), "Closed non-existing scope.");
        this.currentScope = this.scopes.get(this.currentScope).enclosingNode;
    }

    public visitSourceUnit(sourceUnit: SourceUnit): boolean {
        if (!this.scopes.get(sourceUnit)) {
            // By importing, it is possible that the container already exists.
            this.scopes.set(sourceUnit,
                new DeclarationContainer(this.currentScope, this.scopes.get(this.currentScope)));
        }
        this.currentScope = sourceUnit;
        return true;
    }
    public endVisitSourceUnit(sourceUnit: SourceUnit) {
        sourceUnit.annotation.exportedSymbols = this.scopes.get(sourceUnit).declarations;
        this.closeCurrentScope();
    }

    public visitImportDirective(importDirective: ImportDirective): boolean {
        const importee = importDirective.annotation.sourceUnit;
        Debug.assert(!!importee, "");
        if (!this.scopes.get(importee))
            this.scopes.set(importee, new DeclarationContainer());
        this.scopes.set(importDirective, this.scopes.get(importee));
        this.registerDeclaration(importDirective, false);
        return true;
    }

    public visitContractDefinition(contract: ContractDefinition): boolean {
        this.registerDeclaration(contract, true);
        contract.annotation.canonicalName = this.currentCanonicalName;
        return true;
    }
    public endVisitContractDefinition(_node: ContractDefinition) {
        this.closeCurrentScope();
    }
    public visitStructDefinition(struct: StructDefinition): boolean {
        this.registerDeclaration(struct, true);
        struct.annotation.canonicalName = this.currentCanonicalName;
        return true;
    }
    public endVisitStructDefinition(_node: StructDefinition) {
        this.closeCurrentScope();
    }

    public visitEnumDefinition(enumDefinition: EnumDefinition): boolean {
        this.registerDeclaration(enumDefinition, true);
        enumDefinition.annotation.canonicalName = this.currentCanonicalName;
        return true;
    }
    public endVisitEnumDefinition(_node: EnumDefinition) {
        this.closeCurrentScope();
    }
    public visitEnumValue(value: EnumValue): boolean {
        this.registerDeclaration(value, false);
        return true;
    }

    public visitFunctionDefinition(fun: FunctionDefinition): boolean {
        this.registerDeclaration(fun, true);
        this.currentFunction = fun;
        return true;
    }
    public endVisitFunctionDefinition(_node: FunctionDefinition) {
        this.currentFunction = undefined;
        this.closeCurrentScope();
    }

    public visitModifierDefinition(modifier: ModifierDefinition): boolean {
        this.registerDeclaration(modifier, true);
        this.currentFunction = modifier;
        return true;
    }
    public endVisitModifierDefinition(_node: ModifierDefinition) {
        this.currentFunction = undefined;
        this.closeCurrentScope();
    }

    public endVisitVariableDeclarationStatement(variableDeclarationStatement: VariableDeclarationStatement) {
        // Register the local variables with the function
        // This does not fit here perfectly, but it saves us another AST visit.
        Debug.assert(!!this.currentFunction, "Variable declaration without function.");
        for (const varDeclStatement of variableDeclarationStatement.declarations) {
            if (varDeclStatement)
                this.currentFunction.addLocalVariable(varDeclStatement);
        }
    }
    public visitVariableDeclaration(declaration: VariableDeclaration): boolean {
        this.registerDeclaration(declaration, false);
        return true;
    }

    public visitEventDefinition(event: EventDefinition): boolean {
        this.registerDeclaration(event, true);
        return true;
    }
    public endVisitEventDefinition(_node: EventDefinition) {
        this.closeCurrentScope();
    }
}

/**
 * Resolves name references, typenames and sets the (explicitly given) types for all variable
 * declarations.
 */
export class NameAndTypeResolver {
    private currentScope?: DeclarationContainer;

    /// Creates the resolver with the given declarations added to the global scope.
    /// @param _scopes mapping of scopes to be used (usually default constructed), these
    /// are filled during the lifetime of this object.
    constructor(
        globals: Declaration[],
        /// Maps nodes declaring a scope to scopes, i.e. ContractDefinition and FunctionDeclaration,
        /// where nullptr denotes the global scope. Note that structs are not scope since they do
        /// not contain code.
        private readonly scopes: Map<ASTNode, DeclarationContainer>,
        private readonly diagnosticReporter: DiagnosticReporter
    ) {
        if (!scopes.get(null))
            scopes.set(null, new DeclarationContainer());
        for (const declaration of globals)
            scopes.get(null).registerDeclaration(declaration);
    }

    /// Registers all declarations found in the AST node, usually a source unit.
    /// @returns false in case of error.
    /// @param _currentScope should be nullptr but can be used to inject new declarations into
    /// existing scopes, used by the snippets feature.
    public registerDeclarations(sourceUnit: ASTNode, currentScope?: ASTNode) {
        // The helper registers all declarations in m_scopes as a side-effect of its construction.
        try {
            new DeclarationRegistrationHelper(this.scopes, sourceUnit, this.diagnosticReporter, currentScope);
        }
        catch (err) {
            if (this.diagnosticReporter.diagnostics.length === 0)
                throw new Error("Something went wrong"); // Something is weird here, rather throw again.
            return false;
        }
        return true;
    }

    /// Applies the effect of import directives.
    public performImports(sourceUnit: SourceUnit, sourceUnits: SymbolMap<SourceUnit>): boolean {
        const target = this.scopes.get(sourceUnit);
        let error = false;
        for (const node of sourceUnit.nodes) {
            if (node instanceof ImportDirective) {
                const path = node.annotation.absolutePath;
                if (!sourceUnits.has(path)) {
                    this.diagnosticReporter.declarationError(
                        "Import \"" + path + "\" (referenced as \"" + node.path + "\") not found.",
                        node.location);
                    error = true;
                    continue;
                }
                const scope = this.scopes.get(sourceUnits.get(path));
                Debug.assert(!!scope);
                if (node.symbolAliases.length !== 0) {
                    for (const alias of node.symbolAliases) {
                        const declarations = scope.resolveName(alias[0].name, false);
                        if (declarations.length === 0) {
                            this.diagnosticReporter.declarationError(
                                "Declaration \"" +
                                alias[0].name +
                                "\" not found in \"" +
                                path +
                                "\" (referenced as \"" +
                                node.path +
                                "\").",
                                node.location);
                            error = true;
                        }
                        else
                            for (const declaration of declarations) {
                                if (!DeclarationRegistrationHelper.registerDeclaration(
                                    target, declaration, alias[1], node.location, true, this.diagnosticReporter))
                                    error = true;
                            }
                    }

                }
                else if (node.name !== "")
                    scope.declarations.forEach((declarations, name) => {
                        for (const declaration of declarations) {
                            if (!DeclarationRegistrationHelper.registerDeclaration(
                                target, declaration, name, node.location, true, this.diagnosticReporter))
                                error = true;
                        }
                    });
            }
        }
        return !error;
    }

    /// Resolves all names and types referenced from the given AST Node.
    /// This is usually only called at the contract level, but with a bit of care, it can also
    /// be called at deeper levels.
    /// @param _resolveInsideCode if false, does not descend into nodes that contain code.
    /// @returns false in case of error.
    public resolveNamesAndTypes(node: ASTNode, resolveInsideCode = true): boolean {
        try {
            return this.resolveNamesAndTypesInternal(node, resolveInsideCode);
        }
        catch (err) {
            if (this.diagnosticReporter.diagnostics.length === 0)
                throw new Error("Something went wrong"); // Something is weird here, rather throw again.
            return false;
        }
    }

    /// Updates the given global declaration (used for "this"). Not to be used with declarations
    /// that create their own scope.
    /// @returns false in case of error.
    public updateDeclaration(_declaration: Declaration): boolean {
        try {
            this.scopes.get(null).registerDeclaration(_declaration, "", false, true);
            Debug.assert(!_declaration.scope, "Updated declaration outside global scope.");
        }
        catch (err) {
            if (this.diagnosticReporter.diagnostics.length === 0)
                throw new Error("Something went wrong"); // Something is weird here, rather throw again.
            return false;
        }
        return true;
    }

    /// Resolves the given @a _name inside the scope @a _scope. If @a _scope is omitted,
    /// the global scope is used (i.e. the one containing only the pre-defined global variables).
    /// @returns a pointer to the declaration on success or nullptr on failure.
    public resolveName(name: string, scope?: ASTNode): Declaration[] {
        if (!this.scopes.has(scope))
            return [];
        return this.scopes.get(scope).resolveName(name, false);
    }

    /// Resolves a name in the "current" scope. Should only be called during the initial
    /// resolving phase.
    public nameFromCurrentScope(name: string, recursive = true): Declaration[] {
        return this.currentScope.resolveName(name, recursive);
    }

    /// Resolves a path starting from the "current" scope. Should only be called during the initial
    /// resolving phase.
    /// @note Returns a null pointer if any component in the path was not unique or not found.
    public pathFromCurrentScope(path: ReadonlyArray<string>, recursive = true): Declaration | undefined {
        Debug.assert(path.length !== 0);
        let candidates = this.currentScope.resolveName(first(path), recursive);
        for (let i = 1; i < path.length && candidates.length === 1; i++) {
            if (!this.scopes.has(first(candidates)))
                return undefined;
            candidates = this.scopes.get(first(candidates)).resolveName(path[i], false);
        }
        if (candidates.length === 1)
            return first(candidates);
        else
            return undefined;
    }

    /// returns the vector of declarations without repetitions
    public cleanedDeclarations(identifier: Identifier, declarations: Declaration[]): Declaration[] {
        Debug.assert(declarations.length > 1);
        const uniqueFunctions: Declaration[] = [];

        for (const declaration of declarations) {
            Debug.assert(!!declaration);
            // the declaration is functionDefinition, eventDefinition or a VariableDeclaration while declarations > 1
            Debug.assert(
                declaration instanceof FunctionDefinition ||
                declaration instanceof EventDefinition ||
                declaration instanceof VariableDeclaration,
                "Found overloading involving something not a function or a variable."
            );

            let functionType = declaration.functionType(false);
            if (!functionType)
                functionType = declaration.functionType(true);
            Debug.assert(!!functionType, "Failed to determine the function type of the overloaded.");

            for (const parameter of functionType.parameterTypes.concat(functionType.returnParameterTypes))
                if (!parameter)
                    this.diagnosticReporter.fatalDeclarationError(
                        "Function type can not be used in this context.",
                        identifier.location);

            const index = findIndex(uniqueFunctions, d => {
                let newFunctionType = d.functionType(false);
                if (!newFunctionType)
                    newFunctionType = d.functionType(true);
                return newFunctionType && functionType.hasEqualArgumentTypes(newFunctionType);
            });
            if (index === -1)
                uniqueFunctions.push(declaration);
        }
        return uniqueFunctions;
    }

    /// Generate and store warnings about variables that are named like instructions.
    public warnVariablesNamedLikeInstructions() {
        for (const instructionName of instructionNames) {
            const declarations = this.nameFromCurrentScope(instructionName.toLowerCase());
            for (const declaration of declarations) {
                Debug.assert(!!declaration);
                if (declaration instanceof MagicVariableDeclaration)
                    continue; // Don't warn the user for what the user did not.
                this.diagnosticReporter.warning(
                    "Variable is shadowed in inline assembly by an instruction of the same name",
                    declaration.location);
            }
        }
    }

    /// Internal version of @a resolveNamesAndTypes (called from there) throws exceptions on fatal errors.
    private resolveNamesAndTypesInternal(node: ASTNode, resolveInsideCode = true): boolean {
        if (node instanceof ContractDefinition) {
            let success = true;
            this.currentScope = this.scopes.get(node.scope);
            Debug.assert(!!this.currentScope);

            for (const baseContract of node.baseContracts) {
                if (!this.resolveNamesAndTypes(baseContract, true))
                    success = false;
            }

            this.currentScope = this.scopes.get(node);

            if (success) {
                this.linearizeBaseContracts(node);
                const properBases = node.annotation.linearizedBaseContracts.slice(1);

                for (const base of properBases)
                    this.importInheritedScope(base);
            }

            // these can contain code, only resolve parameters for now
            for (const subNode of node.subNodes) {
                this.currentScope = this.scopes.get(node);
                if (!this.resolveNamesAndTypes(subNode, false)) {
                    success = false;
                    break;
                }
            }

            if (!success)
                return false;

            if (!resolveInsideCode)
                return success;

            this.currentScope = this.scopes.get(node);

            // now resolve references inside the code
            for (const subNode of node.subNodes) {
                this.currentScope = this.scopes.get(node);
                if (!this.resolveNamesAndTypes(subNode, true))
                    success = false;
            }
            return success;
        }
        else {
            if (this.scopes.has(node))
                this.currentScope = this.scopes.get(node);
            return new ReferencesResolver(this.diagnosticReporter, this, resolveInsideCode).resolve(node);
        }
    }

    /// Imports all members declared directly in the given contract (i.e. does not import inherited members)
    /// into the current scope if they are not present already.
    private importInheritedScope(base: ContractDefinition) {
        const container = this.scopes.get(base);
        Debug.assert(!!container);
        container.declarations.forEach(declarations => {
            for (const declaration of declarations) {
                // Import if it was declared in the base, is not the constructor and is visible in derived classes
                if (declaration.scope === base && declaration.isVisibleInDerivedContracts())
                    if (!this.currentScope.registerDeclaration(declaration)) {
                        let firstDeclarationLocation: SourceLocation;
                        let secondDeclarationLocation: SourceLocation;
                        const conflictingDeclaration = this.currentScope.conflictingDeclaration(declaration);
                        Debug.assert(!!conflictingDeclaration);

                        // Usual shadowing is not an error
                        if (declaration instanceof VariableDeclaration && conflictingDeclaration instanceof VariableDeclaration)
                            continue;

                        // Usual shadowing is not an error
                        if (declaration instanceof ModifierDefinition && conflictingDeclaration instanceof ModifierDefinition)
                            continue;

                        if (declaration.location.start < conflictingDeclaration.location.start) {
                            firstDeclarationLocation = declaration.location;
                            secondDeclarationLocation = conflictingDeclaration.location;
                        }
                        else {
                            firstDeclarationLocation = conflictingDeclaration.location;
                            secondDeclarationLocation = declaration.location;
                        }

                        this.diagnosticReporter.declarationError(
                            "Identifier already declared.",
                            secondDeclarationLocation,
                            new SecondarySourceLocation().append(firstDeclarationLocation, "The previous declaration is here:")
                        );
                    }
            }
        });
    }

    /// Computes "C3-Linearization" of base contracts and stores it inside the contract. Reports errors if any
    private linearizeBaseContracts(_contract: ContractDefinition) {
        // order in the lists is from derived to base
        // list of lists to linearize, the last element is the list of direct bases
        const input: ContractDefinition[][] = [[]];
        for (const baseSpecifier of _contract.baseContracts) {
            const baseName = baseSpecifier.name;
            const base = baseName.annotation.referencedDeclaration as ContractDefinition;
            if (!base)
                this.diagnosticReporter.fatalTypeError("Contract expected.", baseName.location);
            // "unshift" has the effect that bases mentioned later can overwrite members of bases
            // mentioned earlier
            last(input).unshift(base);
            const basesBases = base.annotation.linearizedBaseContracts;
            if (basesBases.length === 0)
                this.diagnosticReporter.fatalTypeError(
                    "Definition of base has to precede definition of derived contract",
                    baseName.location);
            input.unshift(basesBases.slice());
        }
        last(input).unshift(_contract);
        const result = NameAndTypeResolver.cThreeMerge(input);
        if (result.length === 0)
            this.diagnosticReporter.fatalTypeError("Linearization of inheritance graph impossible", _contract.location);
        _contract.annotation.linearizedBaseContracts = result;
        _contract.annotation.contractDependencies.push(...result.slice(1));
    }

    /// Computes the C3-merge of the given list of lists of bases.
    /// @returns the linearized vector or an empty vector if linearization is not possible.
    private static cThreeMerge<T>(toMerge: T[][]): T[] {
        // returns true iff _candidate appears only as last element of the lists
        function appearsOnlyAtHead(candidate: T): boolean {
            for (const bases of toMerge) {
                Debug.assert(bases.length !== 0);
                if (bases.slice(1).indexOf(candidate) !== -1)
                    return false;
            }
            return true;
        }
        // returns the next candidate to append to the linearized list or nullptr on failure
        function nextCandidate(): T | undefined {
            for (const bases of toMerge) {
                Debug.assert(bases.length !== 0);
                if (appearsOnlyAtHead(first(bases)))
                    return first(bases);
            }
            return undefined;
        }
        // removes the given contract from all lists
        function removeCandidate(candidate: T) {
            for (let i = 0; i < toMerge.length; ++i) {
                const it = toMerge[i];
                const foundIndex = it.indexOf(candidate);
                if (foundIndex !== -1) {
                    it.splice(foundIndex, 1);
                }
                if (it.length === 0)
                    toMerge.splice(i, 1);
            }
        }

        removeWhere(toMerge, (bases: T[]) => bases.length === 0);
        const result: T[] = [];
        while (toMerge.length !== 0) {
            const candidate = nextCandidate();
            if (!candidate)
                return [];
            result.push(candidate);
            removeCandidate(candidate);
        }
        return result;
    }
}

const instructionNames = [
    "STOP",
    "ADD",
    "SUB",
    "MUL",
    "DIV",
    "SDIV",
    "MOD",
    "SMOD",
    "EXP",
    "NOT",
    "LT",
    "GT",
    "SLT",
    "SGT",
    "EQ",
    "ISZERO",
    "AND",
    "OR",
    "XOR",
    "BYTE",
    "ADDMOD",
    "MULMOD",
    "SIGNEXTEND",
    "KECCAK256",
    "ADDRESS",
    "BALANCE",
    "ORIGIN",
    "CALLER",
    "CALLVALUE",
    "CALLDATALOAD",
    "CALLDATASIZE",
    "CALLDATACOPY",
    "CODESIZE",
    "CODECOPY",
    "GASPRICE",
    "EXTCODESIZE",
    "EXTCODECOPY",
    "RETURNDATASIZE",
    "RETURNDATACOPY",
    "BLOCKHASH",
    "COINBASE",
    "TIMESTAMP",
    "NUMBER",
    "DIFFICULTY",
    "GASLIMIT",
    "POP",
    "MLOAD",
    "MSTORE",
    "MSTORE8",
    "SLOAD",
    "SSTORE",
    "JUMP",
    "JUMPI",
    "PC",
    "MSIZE",
    "GAS",
    "JUMPDEST",
    "PUSH1",
    "PUSH2",
    "PUSH3",
    "PUSH4",
    "PUSH5",
    "PUSH6",
    "PUSH7",
    "PUSH8",
    "PUSH9",
    "PUSH10",
    "PUSH11",
    "PUSH12",
    "PUSH13",
    "PUSH14",
    "PUSH15",
    "PUSH16",
    "PUSH17",
    "PUSH18",
    "PUSH19",
    "PUSH20",
    "PUSH21",
    "PUSH22",
    "PUSH23",
    "PUSH24",
    "PUSH25",
    "PUSH26",
    "PUSH27",
    "PUSH28",
    "PUSH29",
    "PUSH30",
    "PUSH31",
    "PUSH32",
    "DUP1",
    "DUP2",
    "DUP3",
    "DUP4",
    "DUP5",
    "DUP6",
    "DUP7",
    "DUP8",
    "DUP9",
    "DUP10",
    "DUP11",
    "DUP12",
    "DUP13",
    "DUP14",
    "DUP15",
    "DUP16",
    "SWAP1",
    "SWAP2",
    "SWAP3",
    "SWAP4",
    "SWAP5",
    "SWAP6",
    "SWAP7",
    "SWAP8",
    "SWAP9",
    "SWAP10",
    "SWAP11",
    "SWAP12",
    "SWAP13",
    "SWAP14",
    "SWAP15",
    "SWAP16",
    "LOG0",
    "LOG1",
    "LOG2",
    "LOG3",
    "LOG4",
    "CREATE",
    "CALL",
    "CALLCODE",
    "STATICCALL",
    "RETURN",
    "DELEGATECALL",
    "CREATE2",
    "REVERT",
    "INVALID",
    "SELFDESTRUCT"
];
