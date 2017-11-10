import { BigNumber as BN } from "bignumber.js";

import {
    ASTNode,
    ArrayType,
    Block,
    ContractDefinition,
    ContractKind,
    ContractType,
    DataLocation,
    EnumDefinition,
    EnumType,
    FixedBytesType,
    FunctionKind,
    FunctionType,
    InaccessibleDynamicType,
    Literal,
    MagicKind,
    MagicType,
    MappingType,
    Member,
    MemberList,
    ModifierDefinition,
    ModifierType,
    ModuleType,
    ParameterList,
    SourceUnit,
    StringLiteralType,
    StructDefinition,
    TupleType,
    Type
} from "../../src/compiler/ast";
import { TokenName } from "../../src/compiler/token";
import { SourceLocation } from "../../src/compiler/types";

test("storage_layout_simple", () => {
    const members = new MemberList([
        new Member("first", Type.fromElementaryTypeName("uint128")),
        new Member("second", Type.fromElementaryTypeName("uint120")),
        new Member("wraps", Type.fromElementaryTypeName("uint16"))
    ]);
    expect(members.storageSize.equals(2));
    expect(members.memberStorageOffset("first")).not.toBeNull();
    expect(members.memberStorageOffset("second")).not.toBeNull();
    expect(members.memberStorageOffset("wraps")).not.toBeNull();
    expect(members.memberStorageOffset("first")).toEqual([new BN(0), 0]);
    expect(members.memberStorageOffset("second")).toEqual([new BN(0), 16]);
    expect(members.memberStorageOffset("wraps")).toEqual([new BN(1), 0]);
});

test("storage_layout_mapping", () => {
    const members = new MemberList([
        new Member("first", Type.fromElementaryTypeName("uint128")),
        new Member("second", new MappingType(
            Type.fromElementaryTypeName("uint8"),
            Type.fromElementaryTypeName("uint8")
        )),
        new Member("third", Type.fromElementaryTypeName("uint16")),
        new Member("final", new MappingType(
            Type.fromElementaryTypeName("uint8"),
            Type.fromElementaryTypeName("uint8")
        ))
    ]);
    expect(members.storageSize).toEqual(new BN(4));
    expect(members.memberStorageOffset("first")).not.toBeNull();
    expect(members.memberStorageOffset("second")).not.toBeNull();
    expect(members.memberStorageOffset("third")).not.toBeNull();
    expect(members.memberStorageOffset("final")).not.toBeNull();
    expect(members.memberStorageOffset("first")).toEqual([new BN(0), 0]);
    expect(members.memberStorageOffset("second")).toEqual([new BN(1), 0]);
    expect(members.memberStorageOffset("third")).toEqual([new BN(2), 0]);
    expect(members.memberStorageOffset("final")).toEqual([new BN(3), 0]);
});

test("storage_layout_arrays", () => {
    expect(ArrayType.newFixedSizeArray(DataLocation.Storage, new FixedBytesType(1), new BN(32)).storageSize).toEqual(new BN(1));
    expect(ArrayType.newFixedSizeArray(DataLocation.Storage, new FixedBytesType(1), new BN(33)).storageSize).toEqual(new BN(2));
    expect(ArrayType.newFixedSizeArray(DataLocation.Storage, new FixedBytesType(2), new BN(31)).storageSize).toEqual(new BN(2));
    expect(ArrayType.newFixedSizeArray(DataLocation.Storage, new FixedBytesType(7), new BN(8)).storageSize).toEqual(new BN(2));
    expect(ArrayType.newFixedSizeArray(DataLocation.Storage, new FixedBytesType(7), new BN(9)).storageSize).toEqual(new BN(3));
    expect(ArrayType.newFixedSizeArray(DataLocation.Storage, new FixedBytesType(31), new BN(9)).storageSize).toEqual(new BN(9));
    expect(ArrayType.newFixedSizeArray(DataLocation.Storage, new FixedBytesType(32), new BN(9)).storageSize).toEqual(new BN(9));
});

test("type_identifiers", () => {
    ASTNode.resetID();
    expect(Type.fromElementaryTypeName("uint128").identifier).toBe("t_uint128");
    expect(Type.fromElementaryTypeName("int128").identifier).toBe("t_int128");
    expect(Type.fromElementaryTypeName("address").identifier).toBe("t_address");
    expect(Type.fromElementaryTypeName("uint8").identifier).toBe("t_uint8");
    expect(Type.fromElementaryTypeName("ufixed8x64").identifier).toBe("t_ufixed8x64");
    expect(Type.fromElementaryTypeName("fixed128x8").identifier).toBe("t_fixed128x8");

    // FIXME: Fix skipped tests.
    // expect(new RationalNumberType(rational(7, 1)).identifier).toBe("t_rational_7_by_1");
    // expect(new RationalNumberType(rational(200, 77)).identifier).toBe("t_rational_200_by_77");
    // expect(new RationalNumberType(rational(2 * 200, 2 * 77)).identifier).toBe("t_rational_200_by_77");
    expect(
        new StringLiteralType(new Literal(new SourceLocation(), TokenName.StringLiteral, "abc - def")).identifier).toBe(
        "t_stringliteral_196a9142ee0d40e274a6482393c762b16dd8315713207365e1e13d8d85b74fc4"
        );
    expect(Type.fromElementaryTypeName("bytes8").identifier).toBe("t_bytes8");
    expect(Type.fromElementaryTypeName("bytes32").identifier).toBe("t_bytes32");
    expect(Type.fromElementaryTypeName("bool").identifier).toBe("t_bool");
    expect(Type.fromElementaryTypeName("bytes").identifier).toBe("t_bytes_storage_ptr");
    expect(Type.fromElementaryTypeName("string").identifier).toBe("t_string_storage_ptr");
    const largeintArray = ArrayType.newFixedSizeArray(DataLocation.Memory, Type.fromElementaryTypeName("int128"), new BN("2535301200456458802993406410752"));
    expect(largeintArray.identifier).toBe("t_array$_t_int128_$2535301200456458802993406410752_memory_ptr");
    const stringArray = ArrayType.newFixedSizeArray(DataLocation.Storage, Type.fromElementaryTypeName("string"), new BN("20"));
    const multiArray = ArrayType.newDynamicSizeArray(DataLocation.Storage, stringArray);
    expect(multiArray.identifier).toBe("t_array$_t_array$_t_string_storage_$20_storage_$dyn_storage_ptr");

    const c = new ContractDefinition(new SourceLocation(), "MyContract$", "", [], [], ContractKind.Contract);
    expect(c.type.identifier).toBe("t_type$_t_contract$_MyContract$$$_$2_$");
    expect(new ContractType(c, true).identifier).toBe("t_super$_MyContract$$$_$2");

    const s = new StructDefinition(new SourceLocation(), "Struct", []);
    expect(s.type.identifier).toBe("t_type$_t_struct$_Struct_$3_storage_ptr_$");

    const e = new EnumDefinition(new SourceLocation(), "Enum", []);
    expect(e.type.identifier).toBe("t_type$_t_enum$_Enum_$4_$");

    const t = new TupleType([e.type, s.type, stringArray, null]);
    expect(t.identifier).toBe("t_tuple$_t_type$_t_enum$_Enum_$4_$_$_t_type$_t_struct$_Struct_$3_storage_ptr_$_$_t_array$_t_string_storage_$20_storage_ptr_$__$");
    const sha3fun = new FunctionType([], [], [], [], FunctionKind.SHA3);
    expect(sha3fun.identifier).toBe("t_function_sha3_nonpayable$__$returns$__$");

    const metaFun = new FunctionType([sha3fun], [s.type]);
    expect(metaFun.identifier).toBe("t_function_internal_nonpayable$_t_function_sha3_nonpayable$__$returns$__$_$returns$_t_type$_t_struct$_Struct_$3_storage_ptr_$_$");

    const m = new MappingType(Type.fromElementaryTypeName("bytes32"), s.type);
    const m2 = new MappingType(Type.fromElementaryTypeName("uint64"), m);
    expect(m2.identifier).toBe("t_mapping$_t_uint64_$_t_mapping$_t_bytes32_$_t_type$_t_struct$_Struct_$3_storage_ptr_$_$_$");

    // // TypeType is tested with contract

    const emptyParams = new ParameterList(new SourceLocation(), []);
    const mod = new ModifierDefinition(new SourceLocation(), "modif", "", emptyParams, undefined);
    expect(new ModifierType(mod).identifier).toBe("t_modifier$__$");

    const su = new SourceUnit(new SourceLocation(), "", []);
    expect(new ModuleType(su).identifier).toBe("t_module_7");
    expect(new MagicType(MagicKind.Block).identifier).toBe("t_magic_block");
    expect(new MagicType(MagicKind.Message).identifier).toBe("t_magic_message");
    expect(new MagicType(MagicKind.Transaction).identifier).toBe("t_magic_transaction");

    expect(new InaccessibleDynamicType().identifier).toBe("t_inaccessible");
});
