import { Debug, createMapFromTemplate, every, findIndex } from "../core";
import { CharacterCodes, Map } from "../types";

export enum TokenName {
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

export function assignmentToBinaryOp(op: TokenName): TokenName {
    Debug.assert(isAssignmentOp(op) && op !== TokenName.Assign);
    return op + (TokenName.BitOr - TokenName.AssignBitOr);
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

export class ElementaryTypeNameToken {
    constructor(
        public readonly token: TokenName,
        public readonly firstNumber: number,
        public readonly secondNumber: number) {
    }

    ///if tokValue is set to true, then returns the actual token type name, otherwise, returns full type
    public toString(tokenValue = false): string {
        const name = tokenToString(this.token);
        if (tokenValue || (this.firstNumber === 0 && this.secondNumber === 0))
            return name;
        Debug.assert(name.length >= 3, "Token name size should be greater than 3. Should not reach here.");
        if (this.token === TokenName.FixedMxN || this.token === TokenName.UFixedMxN)
            return name.substr(0, name.length - 3) + this.firstNumber + "x" + this.secondNumber;
        else
            return name.substr(0, name.length - 1) + this.firstNumber;
    }
}

const textToToken = createMapFromTemplate({
    "(": TokenName.LParen,
    ")": TokenName.RParen,
    "[": TokenName.LBrack,
    "]": TokenName.RBrack,
    "{": TokenName.LBrace,
    "}": TokenName.RBrace,
    ":": TokenName.Colon,
    ";": TokenName.Semicolon,
    ".": TokenName.Period,
    "?": TokenName.Conditional,
    "=>": TokenName.Arrow,
    "=": TokenName.Assign,
    "|=": TokenName.AssignBitOr,
    "^=": TokenName.AssignBitXor,
    "&=": TokenName.AssignBitAnd,
    "<<=": TokenName.AssignShl,
    ">>=": TokenName.AssignSar,
    ">>>=": TokenName.AssignShr,
    "+=": TokenName.AssignAdd,
    "-+": TokenName.AssignSub,
    "*=": TokenName.AssignMul,
    "/=": TokenName.AssignDiv,
    "%=": TokenName.AssignMod,
    ",": TokenName.Comma,
    "||": TokenName.Or,
    "&&": TokenName.And,
    "|": TokenName.BitOr,
    "^": TokenName.BitXor,
    "&": TokenName.BitAnd,
    "<<": TokenName.SHL,
    ">>": TokenName.SAR,
    ">>>": TokenName.SHR,
    "+": TokenName.Add,
    "-": TokenName.Sub,
    "*": TokenName.Mul,
    "/": TokenName.Div,
    "%": TokenName.Mod,
    "**": TokenName.Exp,
    "==": TokenName.Equal,
    "!=": TokenName.NotEqual,
    "<": TokenName.LessThan,
    ">": TokenName.GreaterThan,
    "<=": TokenName.LessThanOrEqual,
    ">=": TokenName.GreaterThanOrEqual,
    "!": TokenName.Not,
    "~": TokenName.BitNot,
    "++": TokenName.Inc,
    "--": TokenName.Dec,
    "delete": TokenName.Delete,
    "anonymous": TokenName.Anonymous,
    "as": TokenName.As,
    "assembly": TokenName.Assembly,
    "break": TokenName.Break,
    "constant": TokenName.Constant,
    "continue": TokenName.Continue,
    "contract": TokenName.Contract,
    "do": TokenName.Do,
    "else": TokenName.Else,
    "enum": TokenName.Enum,
    "event": TokenName.Event,
    "external": TokenName.External,
    "for": TokenName.For,
    "function": TokenName.Function,
    "hex": TokenName.Hex,
    "if": TokenName.If,
    "indexed": TokenName.Indexed,
    "interface": TokenName.Interface,
    "internal": TokenName.Internal,
    "import": TokenName.Import,
    "is": TokenName.Is,
    "library": TokenName.Library,
    "mapping": TokenName.Mapping,
    "memory": TokenName.Memory,
    "modifier": TokenName.Modifier,
    "new": TokenName.New,
    "payable": TokenName.Payable,
    "public": TokenName.Public,
    "pragma": TokenName.Pragma,
    "private": TokenName.Private,
    "pure": TokenName.Pure,
    "return": TokenName.Return,
    "returns": TokenName.Returns,
    "storage": TokenName.Storage,
    "struct": TokenName.Struct,
    "throw": TokenName.Throw,
    "using": TokenName.Using,
    "var": TokenName.Var,
    "view": TokenName.View,
    "while": TokenName.While,
    "wei": TokenName.SubWei,
    "szabo": TokenName.SubSzabo,
    "finney": TokenName.SubFinney,
    "ether": TokenName.SubEther,
    "seconds": TokenName.SubSecond,
    "minutes": TokenName.SubMinute,
    "hours": TokenName.SubHour,
    "days": TokenName.SubDay,
    "weeks": TokenName.SubWeek,
    "years": TokenName.SubYear,
    "int": TokenName.Int,
    "uint": TokenName.UInt,
    "bytes": TokenName.Bytes,
    "byte": TokenName.Byte,
    "string": TokenName.String,
    "address": TokenName.Address,
    "bool": TokenName.Bool,
    "fixed": TokenName.Fixed,
    "ufixed": TokenName.UFixed,
    "intM": TokenName.IntM,
    "uintM": TokenName.UIntM,
    "bytesM": TokenName.BytesM,
    "fixedMxN": TokenName.FixedMxN,
    "ufixedMxN": TokenName.UFixedMxN,
    "true": TokenName.TrueLiteral,
    "false": TokenName.FalseLiteral,
    "abstract": TokenName.Abstract,
    "after": TokenName.After,
    "case": TokenName.Case,
    "catch": TokenName.Catch,
    "default": TokenName.Default,
    "final": TokenName.Final,
    "in": TokenName.In,
    "inline": TokenName.Inline,
    "let": TokenName.Let,
    "match": TokenName.Match,
    "null": TokenName.NullLiteral,
    "of": TokenName.Of,
    "relocatable": TokenName.Relocatable,
    "static": TokenName.Static,
    "switch": TokenName.Switch,
    "try": TokenName.Try,
    "type": TokenName.Type,
    "typeof": TokenName.TypeOf
});

export function precedence(t: TokenName): number {
    switch (t) {
        case TokenName.Exp:
            return 14;
        case TokenName.Mul:
        case TokenName.Div:
        case TokenName.Mod:
            return 13;
        case TokenName.Add:
        case TokenName.Sub:
            return 12;
        case TokenName.SHL:
        case TokenName.SAR:
        case TokenName.SHR:
            return 11;
        case TokenName.BitAnd:
            return 10;
        case TokenName.BitXor:
            return 9;
        case TokenName.BitOr:
            return 8;
        case TokenName.LessThan:
        case TokenName.GreaterThan:
        case TokenName.LessThanOrEqual:
        case TokenName.GreaterThanOrEqual:
            return 7;
        case TokenName.Equal:
        case TokenName.NotEqual:
            return 6;
        case TokenName.And:
            return 5;
        case TokenName.Or:
            return 4;
        case TokenName.Conditional:
            return 3;
        case TokenName.Assign:
        case TokenName.AssignBitOr:
        case TokenName.AssignBitXor:
        case TokenName.AssignBitAnd:
        case TokenName.AssignShl:
        case TokenName.AssignSar:
        case TokenName.AssignShr:
        case TokenName.AssignAdd:
        case TokenName.AssignSub:
        case TokenName.AssignMul:
        case TokenName.AssignDiv:
        case TokenName.AssignMod:
            return 2;
        case TokenName.Comma:
            return 1;
        default:
            return 0;
    }
}

/* @internal */
export function stringToToken(s: string): TokenName {
    return textToToken.get(s);
}

function makeReverseMap(source: Map<number>): string[] {
    const result: string[] = [];
    source.forEach((value, name) => {
        result[value] = name;
    });
    return result;
}

const tokenStrings = makeReverseMap(textToToken);

export function tokenToString(t: TokenName): string | undefined {
    return tokenStrings[t];
}

export function tokenToName(t: TokenName): string {
    return TokenName[t];
}

export function fromIdentifierOrKeyword(literal: number[]): { token: TokenName, m: number, n: number } {
    const isDigit = (c: number) => CharacterCodes._0 <= c && c <= CharacterCodes._9;
    const literalString = String.fromCharCode(...literal);

    const positionM = findIndex(literal, isDigit);
    if (positionM !== -1) {
        const baseType = literalString.substring(0, positionM);
        const positionX = findIndex(literal, c => !isDigit(c), { from: positionM });
        const m = parseInt(literalString.substring(positionM, positionX === -1 ? undefined : positionX));
        const keyword = keywordByName(baseType);
        if (keyword === TokenName.Bytes) {
            if (0 < m && m <= 32 && positionX === -1)
                return { token: TokenName.BytesM, m, n: 0 };
        } else if (keyword === TokenName.UInt || keyword === TokenName.Int) {
            if (0 < m && m <= 256 && m % 8 === 0 && positionX === -1) {
                if (keyword === TokenName.UInt)
                    return { token: TokenName.UIntM, m, n: 0 };
                else
                    return { token: TokenName.IntM, m, n: 0 };
            }
        } else if (keyword === TokenName.UFixed || keyword === TokenName.Fixed) {
            if (
                positionM < positionX &&
                positionX !== -1 &&
                literal[positionX] === CharacterCodes.x &&
                every(literal, isDigit, { from: positionX + 1 })
            ) {
                const n = parseInt(literalString.substring(positionX + 1));
                if (8 <= m && m <= 256 && m % 8 === 0 && 0 <= n && n <= 80) {
                    if (keyword === TokenName.UFixed)
                        return { token: TokenName.UFixedMxN, m, n };
                    else
                        return { token: TokenName.FixedMxN, m, n };
                }
            }
        }
        return { token: TokenName.Identifier, m: 0, n: 0 };
    }

    return {
        token: keywordByName(literalString),
        m: 0,
        n: 0
    };
}

function keywordByName(name: string): TokenName {
    const token = stringToToken(name);
    return token ? token : TokenName.Identifier;
}
