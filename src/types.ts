/**
 * Type of objects whose values are all of the same type.
 * The `in` and `for-in` operators can *not* be safely used,
 * since `Object.prototype` may be modified by outside code.
 */
export interface MapLike<T> {
    [index: string]: T;
}

/** ES6 Map interface, only read methods included. */
export interface ReadonlyMap<T> {
    get(key: string): T | undefined;
    has(key: string): boolean;
    forEach(action: (value: T, key: string) => void): void;
    readonly size: number;
    keys(): Iterator<string>;
    values(): Iterator<T>;
    entries(): Iterator<[string, T]>;
}

/** ES6 Map interface. */
export interface Map<T> extends ReadonlyMap<T> {
    set(key: string, value: T): this;
    delete(key: string): boolean;
    clear(): void;
}

export const hasOwnProperty = Object.prototype.hasOwnProperty;

// The global Map object. This may not be available, so we must test for it.
declare const Map: { new <T>(): Map<T> } | undefined;
export const MapCtr = Map;

export const enum TokenName {
    // End of source indicator.
    EOS,

    // Punctuators (ECMA-262, section 7.7, page 15).
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Colon,
    Semicolon,
    Period,
    Conditional,
    Arrow,
    Assign,
    AssignBitOr,
    AssignBitXor,
    AssignBitAnd,
    AssignShl,
    AssignSar,
    AssignShr,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,

    // Binary operators sorted by precedence.
    Comma,
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    SHL,
    SAR,
    SHR,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,

    // Compare operators sorted by precedence.
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,

    // Unary operators.
    Not,
    BitNot,
    Inc,
    Dec,
    Delete,

    // Keywords
    Anonymous,
    As,
    Assembly,
    Break,
    Constant,
    Continue,
    Contract,
    Do,
    Else,
    Enum,
    Event,
    External,
    For,
    Function,
    Hex,
    If,
    Indexed,
    Interface,
    Internal,
    Import,
    Is,
    Library,
    Mapping,
    Memory,
    Modifier,
    New,
    Payable,
    Public,
    Pragma,
    Private,
    Pure,
    Return,
    Returns,
    Storage,
    Struct,
    Throw,
    Using,
    Var,
    View,
    While,

    // Ether subdenominations
    SubWei,
    SubSzabo,
    SubFinney,
    SubEther,
    SubSecond,
    SubMinute,
    SubHour,
    SubDay,
    SubWeek,
    SubYear,
    // type keywords
    Int,
    UInt,
    Bytes,
    Byte,
    String,
    Address,
    Bool,
    Fixed,
    UFixed,
    IntM,
    UIntM,
    BytesM,
    FixedMxN,
    UFixedMxN,
    TypesEnd,

    // Literals
    TrueLiteral,
    FalseLiteral,
    Number,
    StringLiteral,
    CommentLiteral,

    // Identifiers (not keywords or future reserved words).
    Identifier,

    // Keywords reserved for future use.
    Abstract,
    After,
    Case,
    Catch,
    Default,
    Final,
    In,
    Inline,
    Let,
    Match,
    NullLiteral,
    Of,
    Relocatable,
    Static,
    Switch,
    Try,
    Type,
    TypeOf,

    // Illegal token - not able to scan.
    Illegal,

    // Scanner-internal use only.
    Whitespace
};

export function isElementaryTypeName(tok: TokenName): boolean {
    return TokenName.Int <= tok && tok < TokenName.TypesEnd;
}

export function isAssignmentOp(tok: TokenName): boolean {
    return TokenName.Assign <= tok && tok <= TokenName.AssignMod;
}

export function isBinaryOp(op: TokenName): boolean {
    return TokenName.Comma <= op && op <= TokenName.Exp;
}

export function isCommutativeOp(op: TokenName): boolean {
    return op === TokenName.BitOr || op === TokenName.BitXor || op === TokenName.BitAnd ||
        op === TokenName.Add || op === TokenName.Mul || op === TokenName.Equal || op === TokenName.NotEqual;
}

export function isArithmeticOp(op: TokenName): boolean {
    return TokenName.Add <= op && op <= TokenName.Exp;
}

export function isCompareOp(op: TokenName): boolean {
    return TokenName.Equal <= op && op <= TokenName.GreaterThanOrEqual;
}

export function isBitOp(op: TokenName): boolean {
    return (TokenName.BitOr <= op && op <= TokenName.BitAnd) || op === TokenName.BitNot;
}

export function isBooleanOp(op: TokenName): boolean {
    return (TokenName.Or <= op && op <= TokenName.And) || op === TokenName.Not;
}

export function isUnaryOp(op: TokenName): boolean {
    return (TokenName.Not <= op && op <= TokenName.Delete) || op === TokenName.Add || op === TokenName.Sub;
}

export function isCountOp(op: TokenName): boolean {
    return op === TokenName.Inc || op === TokenName.Dec;
}

export function isShiftOp(op: TokenName): boolean {
    return (TokenName.SHL <= op) && (op <= TokenName.SHR);
}

export function isVisibilitySpecifier(op: TokenName): boolean {
    return isVariableVisibilitySpecifier(op) || op === TokenName.External;
}

export function isVariableVisibilitySpecifier(op: TokenName): boolean {
    return op === TokenName.Public || op === TokenName.Private || op === TokenName.Internal;
}

export function isLocationSpecifier(op: TokenName): boolean {
    return op === TokenName.Memory || op === TokenName.Storage;
}

export function isStateMutabilitySpecifier(op: TokenName): boolean {
    return op === TokenName.Pure || op === TokenName.Constant || op === TokenName.View || op === TokenName.Payable;
}

export function isEtherSubdenomination(op: TokenName): boolean {
    return op === TokenName.SubWei || op === TokenName.SubSzabo || op === TokenName.SubFinney || op === TokenName.SubEther;
}

export function isTimeSubdenomination(op: TokenName): boolean {
    return op === TokenName.SubSecond || op === TokenName.SubMinute || op === TokenName.SubHour || op === TokenName.SubDay || op === TokenName.SubWeek || op === TokenName.SubYear;
}

export function isReservedKeyword(op: TokenName): boolean {
    return (TokenName.Abstract <= op && op <= TokenName.TypeOf);
}

export class ElementTypeNameToken {
    constructor(
        public readonly token: TokenName,
        public readonly firstNumber: number,
        public readonly secondNumber: number) {
    }
}
