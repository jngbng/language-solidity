import {
    ContractDefinition,
    ContractType,
    Declaration,
    FunctionKind,
    FunctionType,
    IntegerType,
    MagicKind,
    MagicType,
    MagicVariableDeclaration,
    StateMutability
} from "../ast/ast";

/**
 * Container for all global objects which look like AST nodes, but are not part of the AST
 * that is currently being compiled.
 * @note must not be destroyed or moved during compilation as its objects can be referenced from
 * other objects.
 */
export class GlobalContext {
    private magicVariables: MagicVariableDeclaration[];

    private currentContract?: ContractDefinition;

    private thisPointer: Map<ContractDefinition, MagicVariableDeclaration> = new Map();;

    private superPointer: Map<ContractDefinition, MagicVariableDeclaration> = new Map();;

    constructor() {
        this.magicVariables = [
            new MagicVariableDeclaration("addmod", new FunctionType([
                "uint256", "uint256", "uint256"],
                ["uint256"],
                [],
                [],
                FunctionKind.AddMod,
                false,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("assert", new FunctionType(
                ["bool"],
                [],
                [],
                [],
                FunctionKind.Assert,
                false,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("block", new MagicType(MagicKind.Block)),
            new MagicVariableDeclaration("ecrecover", new FunctionType(
                ["bytes32", "uint8", "bytes32", "bytes32"],
                ["address"],
                [],
                [],
                FunctionKind.ECRecover,
                false,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("keccak256", new FunctionType(
                [],
                ["bytes32"],
                [],
                [],
                FunctionKind.SHA3,
                true,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("log0", new FunctionType(
                ["bytes32"],
                [],
                [],
                [],
                FunctionKind.Log0
            )),
            new MagicVariableDeclaration("log1", new FunctionType(
                ["bytes32", "bytes32"],
                [],
                [],
                [],
                FunctionKind.Log1
            )),
            new MagicVariableDeclaration("log2", new FunctionType(
                ["bytes32", "bytes32", "bytes32"],
                [],
                [],
                [],
                FunctionKind.Log2
            )),
            new MagicVariableDeclaration("log3", new FunctionType(
                ["bytes32", "bytes32", "bytes32", "bytes32"],
                [],
                [],
                [],
                FunctionKind.Log3
            )),
            new MagicVariableDeclaration("log4", new FunctionType(
                ["bytes32", "bytes32", "bytes32", "bytes32", "bytes32"],
                [],
                [],
                [],
                FunctionKind.Log4
            )),
            new MagicVariableDeclaration("msg", new MagicType(MagicKind.Message)),
            new MagicVariableDeclaration("mulmod", new FunctionType(
                ["uint256", "uint256", "uint256"],
                ["uint256"],
                [],
                [],
                FunctionKind.MulMod,
                false,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("now", new IntegerType(256)),
            new MagicVariableDeclaration("require", new FunctionType(
                ["bool"],
                [],
                [],
                [],
                FunctionKind.Require,
                false,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("revert", new FunctionType(
                [],
                [],
                [],
                [],
                FunctionKind.Revert,
                false,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("ripemd160", new FunctionType(
                [],
                ["bytes20"],
                [],
                [],
                FunctionKind.RIPEMD160,
                true,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("selfdestruct", new FunctionType(
                ["address"],
                [],
                [],
                [],
                FunctionKind.Selfdestruct
            )),
            new MagicVariableDeclaration("sha256", new FunctionType(
                [],
                ["bytes32"],
                [],
                [],
                FunctionKind.SHA256,
                true,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("sha3", new FunctionType(
                [],
                ["bytes32"],
                [],
                [],
                FunctionKind.SHA3,
                true,
                StateMutability.Pure
            )),
            new MagicVariableDeclaration("suicide", new FunctionType(
                ["address"],
                [],
                [],
                [],
                FunctionKind.Selfdestruct
            )),
            new MagicVariableDeclaration("tx", new MagicType(MagicKind.Transaction))
        ];
    }

    public setCurrentContract(contract: ContractDefinition) {
        this.currentContract = contract;
    }

    public get currentThis(): MagicVariableDeclaration {
        if (!this.thisPointer.has(this.currentContract))
            this.thisPointer.set(this.currentContract, new MagicVariableDeclaration("this", new ContractType(this.currentContract)));
        return this.thisPointer.get(this.currentContract);
    }

    public get currentSuper(): MagicVariableDeclaration {
        if (!this.superPointer.has(this.currentContract))
            this.superPointer.set(this.currentContract, new MagicVariableDeclaration("super", new ContractType(this.currentContract, true)));
        return this.superPointer.get(this.currentContract);
    }

    /// @returns a vector of all implicit global declarations excluding "this".
    public get declarations(): Declaration[] {
        const declarations: Declaration[] = [];
        for (const variable of this.magicVariables)
            declarations.push(variable);
        return declarations;
    }
}
