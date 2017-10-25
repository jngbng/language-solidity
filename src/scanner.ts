import { createMapFromTemplate } from "./core";
import { CharacterCodes, Map, TokenName } from "./types";

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

function isDecimalDigit(c: number): boolean {
    return CharacterCodes._0 <= c && c <= CharacterCodes._9;
}

function isHexDigit(c: number): boolean {
    return isDecimalDigit(c)
        || (CharacterCodes.a <= c && c <= CharacterCodes.f)
        || (CharacterCodes.A <= c && c <= CharacterCodes.F);
}

function isLineTerminator(c: number): boolean {
    return c === CharacterCodes.lineFeed;
}

function isWhiteSpace(c: number): boolean {
    return c === CharacterCodes.space || c === CharacterCodes.lineFeed || c === CharacterCodes.tab || c === CharacterCodes.carriageReturn;
}

function isIdentifierStart(c: number): boolean {
    return c === CharacterCodes._ || c === CharacterCodes.$ || (CharacterCodes.a <= c && c <= CharacterCodes.z) || (CharacterCodes.A <= c && c <= CharacterCodes.Z);
}

function isIdentifierPart(c: number): boolean {
    return isIdentifierStart(c) || isDecimalDigit(c);
}

function hexValue(c: number): number {
    if (c >= CharacterCodes._0 && c <= CharacterCodes._9)
        return c - CharacterCodes._0;
    else if (c >= CharacterCodes.a && c <= CharacterCodes.f)
        return c - CharacterCodes.a + 10;
    else if (c >= CharacterCodes.A && c <= CharacterCodes.F)
        return c - CharacterCodes.A + 10;
    else return -1;
}