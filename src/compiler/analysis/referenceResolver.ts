import {
    ASTNode,
    ArrayType,
    ArrayTypeName,
    Block,
    ContractDefinition,
    ContractType,
    DataLocation,
    Declaration,
    ElementaryTypeName,
    EnumDefinition,
    EnumType,
    FunctionDefinition,
    FunctionType,
    FunctionTypeName,
    Identifier,
    Location,
    Mapping,
    MappingType,
    ModifierDefinition,
    ParameterList,
    RationalNumberType,
    ReferenceType,
    Return,
    StructDefinition,
    StructType,
    Type,
    UserDefinedTypeName,
    VariableDeclaration,
    Visibility
} from "../ast/ast";
import { ASTVisitor } from "../ast/astVisitor";
import { ExperimentalFeature } from "../ast/experimentalFeatures";
import { Debug, first, last } from "../core";
import { DiagnosticReporter } from "../interface/diagnosticReporter";
import { SourceLocation } from "../types";
import { ConstantEvaluator } from "./constantEvaluator";
import { NameAndTypeResolver } from "./nameAndTypeResolver";

export class ReferencesResolver extends ASTVisitor {
    /// Stack of return parameters.
    private returnParameters: ParameterList[];
    private errorOccurred = false;

    constructor(
        private diagnosticReporter: DiagnosticReporter,
        private resolver: NameAndTypeResolver,
        private resolveInsideCode = false
    ) {
        super();
    }

    /// @returns true if no errors during resolving and throws exceptions on fatal errors.
    public resolve(root: ASTNode): boolean {
        root.accept(this);
        return !this.errorOccurred;
    }

    /// Adds a new error to the list of errors.
    private typeError(description: string, location: SourceLocation) {
        this.errorOccurred = true;
        this.diagnosticReporter.typeError(description, location);
    }

    /// Adds a new error to the list of errors and throws to abort reference resolving.
    private fatalTypeError(description: string, location: SourceLocation) {
        this.errorOccurred = true;
        this.diagnosticReporter.fatalTypeError(description, location);
    }

    /// Adds a new error to the list of errors.
    public declarationError(description: string, location: SourceLocation) {
        this.errorOccurred = true;
        this.diagnosticReporter.declarationError(description, location);
    }

    /// Adds a new error to the list of errors and throws to abort reference resolving.
    public fatalDeclarationError(description: string, location: SourceLocation) {
        this.errorOccurred = true;
        this.diagnosticReporter.fatalDeclarationError(description, location);
    }

    public visitBlock(_node: Block): boolean {
        return this.resolveInsideCode
    }

    public visitIdentifier(identifier: Identifier): boolean {
        const declarations = this.resolver.nameFromCurrentScope(identifier.name);
        if (declarations.length === 0)
            this.fatalDeclarationError("Undeclared identifier.", identifier.location);
        else if (declarations.length === 1)
            identifier.annotation.referencedDeclaration = first(declarations);
        else
            identifier.annotation.overloadedDeclarations =
                this.resolver.cleanedDeclarations(identifier, declarations);
        return false;
    }

    public visitElementaryTypeName(typeName: ElementaryTypeName): boolean {
        typeName.annotation.type = Type.fromElementaryTypeName(typeName.typeName);
        return true;
    }

    public visitFunctionDefinition(functionDefinition: FunctionDefinition): boolean {
        this.returnParameters.push(functionDefinition.returnParameterList);
        return true;
    }
    public endVisitFunctionDefinition(_node: FunctionDefinition) {
        Debug.assert(this.returnParameters.length !== 0);
        this.returnParameters.pop();
    }

    public visitModifierDefinition(_node: ModifierDefinition): boolean {
        this.returnParameters.push(undefined);
        return true;
    }
    public endVisitModifierDefinition(_node: ModifierDefinition) {
        Debug.assert(this.returnParameters.length !== 0);
        this.returnParameters.pop();
    }

    public endVisitUserDefinedTypeName(typeName: UserDefinedTypeName) {
        const declaration = this.resolver.pathFromCurrentScope(typeName.namePath);
        if (!declaration)
            this.fatalDeclarationError("Identifier not found or not unique.", typeName.location);

        typeName.annotation.referencedDeclaration = declaration;

        if (declaration instanceof StructDefinition)
            typeName.annotation.type = new StructType(declaration);
        else if (declaration instanceof EnumDefinition)
            typeName.annotation.type = new EnumType(declaration);
        else if (declaration instanceof ContractDefinition)
            typeName.annotation.type = new ContractType(declaration);
        else
            this.fatalTypeError("Name has to refer to a struct, enum or contract.", typeName.location);
    }

    public endVisitFunctionTypeName(typeName: FunctionTypeName) {
        switch (typeName.visibility) {
            case Visibility.Internal:
            case Visibility.External:
                break;
            default:
                this.fatalTypeError("Invalid visibility, can only be \"external\" or \"internal\".", typeName.location);
        }

        if (typeName.isPayable() && typeName.visibility !== Visibility.External)
            this.fatalTypeError("Only external function types can be payable.", typeName.location);
        if (typeName.visibility === Visibility.External) {
            const ts: VariableDeclaration[] = [].concat(typeName.parameterTypes, typeName.returnParameterTypes);
            for (const t of ts) {
                Debug.assert(!!t.annotation.type, "Type not set for parameter.");
                if (!t.annotation.type.canBeUsedExternally(false))
                    this.fatalTypeError("Internal type cannot be used for external function type.", t.location);
            }
        }

        typeName.annotation.type = FunctionType.newFromFunctionTypeName(typeName);
    }

    public endVisitMapping(mapping: Mapping) {
        let keyType = mapping.keyType.annotation.type;
        let valueType = mapping.valueType.annotation.type;
        // Convert key type to memory.
        keyType = ReferenceType.copyForLocationIfReference(DataLocation.Memory, keyType);
        // Convert value type to storage reference.
        valueType = ReferenceType.copyForLocationIfReference(DataLocation.Storage, valueType);
        mapping.annotation.type = new MappingType(keyType, valueType);
    }

    public endVisitArrayTypeName(typeName: ArrayTypeName) {
        const baseType = typeName.baseType.annotation.type;
        if (baseType.storageBytes === 0)
            this.fatalTypeError("Illegal base type of storage size zero for array.", typeName.baseType.location);
        const length = typeName.length;
        if (length) {
            if (!length.annotation.type)
                new ConstantEvaluator(length, this.diagnosticReporter);
            const lengthType = length.annotation.type as RationalNumberType;
            if (!lengthType || !lengthType.mobileType)
                this.fatalTypeError("Invalid array length, expected integer literal.", length.location);
            else if (lengthType.isFractional())
                this.fatalTypeError("Array with fractional length specified.", length.location);
            else if (lengthType.isNegative())
                this.fatalTypeError("Array with negative length specified.", length.location);
            else
                typeName.annotation.type = ArrayType.newFixedSizeArray(DataLocation.Storage, baseType, lengthType.literalValue());
        }
        else
            typeName.annotation.type = ArrayType.newDynamicSizeArray(DataLocation.Storage, baseType);
    }

    public visitReturn(ret: Return): boolean {
        Debug.assert(this.returnParameters.length !== 0);
        ret.annotation.functionReturnParameters = last(this.returnParameters);
        return true;
    }
    public endVisitVariableDeclaration(_variable: VariableDeclaration) {
        if (_variable.annotation.type)
            return;

        let type: Type;
        if (_variable.typeName) {
            type = _variable.typeName.annotation.type;
            const varLoc = _variable.referenceLocation;
            let typeLoc = DataLocation.Memory;
            // References are forced to calldata for external function parameters (not return)
            // and memory for parameters (also return) of publicly visible functions.
            // They default to memory for function parameters and storage for local variables.
            // As an exception, "storage" is allowed for library functions.
            if (type instanceof ReferenceType) {
                let isPointer = true;
                if (_variable.isExternalCallableParameter()) {
                    const contract = (_variable.scope as Declaration).scope as ContractDefinition;
                    if (contract.isLibrary()) {
                        if (varLoc === Location.Memory)
                            this.fatalTypeError(
                                "Location has to be calldata or storage for external " +
                                "library functions (remove the \"memory\" keyword).",
                                _variable.location);
                    }
                    else {
                        // force location of external function parameters (not return) to calldata
                        if (varLoc !== Location.Default)
                            this.fatalTypeError(
                                "Location has to be calldata for external functions " +
                                "(remove the \"memory\" or \"storage\" keyword).",
                                _variable.location);
                    }
                    if (varLoc === Location.Default)
                        typeLoc = DataLocation.CallData;
                    else
                        typeLoc = varLoc === Location.Memory ? DataLocation.Memory : DataLocation.Storage;
                }
                else if (_variable.isCallableParameter() && (_variable.scope as Declaration).isPublic()) {
                    const contract = (_variable.scope as Declaration).scope as ContractDefinition;
                    // force locations of public or external function (return) parameters to memory
                    if (varLoc == Location.Storage && !contract.isLibrary())
                        this.fatalTypeError(
                            "Location has to be memory for publicly visible functions " +
                            "(remove the \"storage\" keyword).",
                            _variable.location);
                    if (varLoc === Location.Default || !contract.isLibrary())
                        typeLoc = DataLocation.Memory;
                    else
                        typeLoc = varLoc == Location.Memory ? DataLocation.Memory : DataLocation.Storage;
                }
                else {
                    if (_variable.isConstant()) {
                        if (varLoc !== Location.Default && varLoc !== Location.Memory)
                            this.fatalTypeError(
                                "Storage location has to be \"memory\" (or unspecified) for constants.",
                                _variable.location);
                        typeLoc = DataLocation.Memory;
                    }
                    else if (varLoc === Location.Default) {
                        if (_variable.isCallableParameter())
                            typeLoc = DataLocation.Memory;
                        else {
                            typeLoc = DataLocation.Storage;
                            if (_variable.isLocalVariable()) {
                                if (_variable.sourceUnit.annotation.experimentalFeatures.has(ExperimentalFeature.V050))
                                    this.typeError(
                                        "Storage location must be specified as either \"memory\" or \"storage\".",
                                        _variable.location);
                                else
                                    this.diagnosticReporter.warning(
                                        "Variable is declared as a storage pointer. " +
                                        "Use an explicit \"storage\" keyword to silence this warning.",
                                        _variable.location);
                            }
                        }
                    }
                    else
                        typeLoc = varLoc === Location.Memory ? DataLocation.Memory : DataLocation.Storage;
                    isPointer = !_variable.isStateVariable();
                }

                type = type.copyForLocation(typeLoc, isPointer);
            }
            else if (varLoc !== Location.Default && !type)
                this.fatalTypeError(
                    "Storage location can only be given for array or struct types.",
                    _variable.location);

            if (!type)
                this.fatalTypeError("Invalid type name.", _variable.location);

        }
        else if (!_variable.canHaveAutoType())
            this.fatalTypeError("Explicit type needed.", _variable.location);
        // otherwise we have a "var"-declaration whose type is resolved by the first assignment

        _variable.annotation.type = type;
    }
}
