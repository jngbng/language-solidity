import { Debug, clone, } from "../core";
import { Token } from "../parsing/token";
import { CharacterCodes, SourceLocation } from "../types";

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

export class CharStream {
    private _position = 0;

    public get position(): number {
        return this._position;
    }

    constructor(public readonly source = "") {
    }

    public get(charsForward = 0): number {
        return this.source.charCodeAt(this.position + charsForward);
    }

    public advanceAndGet(chars = 1): number {
        if (this.isPastEndOfInput())
            return 0;
        this._position += chars;
        if (this.isPastEndOfInput())
            return 0;
        return this.source.charCodeAt(this._position);
    }

    public rollback(amount: number): number {
        Debug.assert(this.position >= amount);
        this._position -= amount;
        return this.get();
    }

    public isPastEndOfInput(charsForward = 0): boolean {
        return (this.position + charsForward) >= this.source.length;
    }

    public reset() {
        this._position = 0;
    }
}

export class TokenDesc {
    token: Token.TokenName;
    location: SourceLocation;
    literal: number[];
    extendedTokenInfo: { m: number, n: number };

    constructor() {
        this.token = Token.TokenName.EOS;
        this.location = new SourceLocation(0, 0);
        this.literal = [];
        this.extendedTokenInfo = { m: 0, n: 0 };
    }

    public clone(): TokenDesc {
        const instance = new TokenDesc();
        instance.token = this.token;
        instance.location = clone(this.location);
        instance.literal = this.literal.slice();
        instance.extendedTokenInfo = clone(this.extendedTokenInfo);
        return instance;
    }
}

enum LiteralType {
    String,
    Number, // not really different from string type in behaviour
    Comment
};

enum NumberKind {
    Decimal,
    Hex,
    Binary
};

class LiteralScope {
    private _complete = false;

    constructor(public readonly scanner: Scanner, public readonly type: LiteralType) {
        if (type === LiteralType.Comment)
            scanner.nextSkippedComment.literal = [];
        else
            scanner.nextToken.literal = [];
    }

    public complete() {
        this._complete = true;
    }

    public destroy() {
        if (!this.complete) {
            if (this.type === LiteralType.Comment)
                this.scanner.nextSkippedComment.literal = [];
            else
                this.scanner.nextToken.literal = [];
        }
    }
}

export class Scanner {
    private skippedComment = new TokenDesc();               // desc for current skipped comment
    /* internal */ nextSkippedComment = new TokenDesc();    // desc for next skiped comment

    private _currentToken = new TokenDesc();    // desc for current token (as returned by Next())
    /* internal */ nextToken = new TokenDesc(); // desc for next token (one token look-ahead)

    private source: CharStream;
    private _sourceName: string;

    /// one character look-ahead, equals 0 at end of input
    private char: number;

    public get sourceName(): string {
        return this._sourceName;
    }

    constructor(source = new CharStream(), sourceName = "") {
        this.resetSource(source, sourceName);
    }

    /// Resets the scanner as if newly constructed with source and sourceName as input.
    public resetSource(source: CharStream, sourceName: string) {
        this.source = source;
        this._sourceName = sourceName;
        this.reset();
    }

    /// Resets scanner to the start of input.
    public reset() {
        this.source.reset();
        this.char = this.source.get();
        this.skipWhitespace();
        this.scanToken();
        this.next();
    }

    /// Returns the next token and advances input.
    public next(): Token.TokenName {
        this._currentToken = this.nextToken.clone();
        this.skippedComment = this.nextSkippedComment.clone();
        this.scanToken();

        return this._currentToken.token;
    }

    public get currentToken(): Token.TokenName {
        return this._currentToken.token;
    }

    public get currentElementaryTypeNameToken(): Token.ElementaryTypeNameToken {
        const { m, n } = this._currentToken.extendedTokenInfo;
        return new Token.ElementaryTypeNameToken(this._currentToken.token, m, n);
    }

    public get currentLocation(): SourceLocation {
        return this._currentToken.location;
    }

    public get currentLiteral(): string {
        return String.fromCharCode(...this._currentToken.literal);
    }

    public get currentTokenInfo(): { m: number, n: number } {
        return this._currentToken.extendedTokenInfo;
    }

    public get currentCommentLocation(): SourceLocation {
        return this.skippedComment.location;
    }
    public get currentCommentLiteral(): string {
        return String.fromCharCode(...this.skippedComment.literal);
    }
    /// Called by the parser during FunctionDefinition parsing to clear the current comment
    public clearCurrentCommentLiteral() {
        this.skippedComment.literal = [];
    }

    /// @returns the next token without advancing input.
    public peekNextToken(): Token.TokenName {
        return this.nextToken.token;
    }

    public peekLocation(): SourceLocation {
        return this.nextToken.location;
    }
    public peekLiteral(): string {
        return String.fromCharCode(...this.nextToken.literal);
    }

    /// Return the current source position.
    private get sourcePos(): number {
        return this.source.position;
    }

    private scanToken() {
        this.nextToken.literal = [];
        this.nextSkippedComment.literal = [];

        let token: Token.TokenName;
        let m, n: number;

        do {
            // Remember the position of the next token
            this.nextToken.location.start = this.sourcePos;
            switch (this.char) {
                case CharacterCodes.lineFeed:
                case CharacterCodes.space:
                case CharacterCodes.tab:
                    token = this.selectToken(Token.TokenName.Whitespace);
                    break;
                case CharacterCodes.doubleQuote:
                case CharacterCodes.singleQuote:
                    token = this.scanString();
                    break;
                case CharacterCodes.lessThan:
                    // < <= << <<=
                    this.advance();
                    if (this.char as number === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.LessThanOrEqual);
                    else if (this.char === CharacterCodes.lessThan)
                        token = this.selectTokenAlt(CharacterCodes.equals, Token.TokenName.AssignShl, Token.TokenName.SHL);
                    else
                        token = Token.TokenName.LessThan;
                    break;
                case CharacterCodes.greaterThan:
                    // > >= >> >>= >>> >>>=
                    this.advance();
                    if (this.char as number === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.GreaterThanOrEqual);
                    else if (this.char === CharacterCodes.greaterThan) {
                        // >> >>= >>> >>>=
                        this.advance();
                        if (this.char as number === CharacterCodes.equals)
                            token = this.selectToken(Token.TokenName.AssignSar);
                        else if (this.char === CharacterCodes.greaterThan)
                            token = this.selectTokenAlt(CharacterCodes.equals, Token.TokenName.AssignShr, Token.TokenName.SHR);
                        else
                            token = Token.TokenName.SAR;
                    }
                    else
                        token = Token.TokenName.GreaterThan;
                    break;
                case CharacterCodes.equals:
                    // = == =>
                    this.advance();
                    if (this.char === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.Equal);
                    else if (this.char === CharacterCodes.greaterThan)
                        token = this.selectToken(Token.TokenName.Arrow);
                    else
                        token = Token.TokenName.Assign;
                    break;
                case CharacterCodes.exclamation:
                    // ! !=
                    this.advance();
                    if (this.char as number === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.NotEqual);
                    else
                        token = Token.TokenName.Not;
                    break;
                case CharacterCodes.plus:
                    // + ++ +=
                    this.advance();
                    if (this.char === CharacterCodes.plus)
                        token = this.selectToken(Token.TokenName.Inc);
                    else if (this.char === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.AssignAdd);
                    else
                        token = Token.TokenName.Add;
                    break;
                case CharacterCodes.minus:
                    // - -- -=
                    this.advance();
                    if (this.char === CharacterCodes.minus)
                        token = this.selectToken(Token.TokenName.Dec);
                    else if (this.char === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.AssignSub);
                    else
                        token = Token.TokenName.Sub;
                    break;
                case CharacterCodes.asterisk:
                    // * ** *=
                    this.advance();
                    if (this.char === CharacterCodes.asterisk)
                        token = this.selectToken(Token.TokenName.Exp);
                    else if (this.char === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.AssignMul);
                    else
                        token = Token.TokenName.Mul;
                    break;
                case CharacterCodes.percent:
                    // % %=
                    token = this.selectTokenAlt(CharacterCodes.equals, Token.TokenName.AssignMod, Token.TokenName.Mod);
                    break;
                case CharacterCodes.slash:
                    // /  // /* /=
                    token = this.scanSlash();
                    break;
                case CharacterCodes.ampersand:
                    // & && &=
                    this.advance();
                    if (this.char === CharacterCodes.ampersand)
                        token = this.selectToken(Token.TokenName.And);
                    else if (this.char === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.AssignBitAnd);
                    else
                        token = Token.TokenName.BitAnd;
                    break;
                case CharacterCodes.bar:
                    // | || |=
                    this.advance();
                    if (this.char === CharacterCodes.bar)
                        token = this.selectToken(Token.TokenName.Or);
                    else if (this.char === CharacterCodes.equals)
                        token = this.selectToken(Token.TokenName.AssignBitOr);
                    else
                        token = Token.TokenName.BitOr;
                    break;
                case CharacterCodes.caret:
                    // ^ ^=
                    token = this.selectTokenAlt(CharacterCodes.equals, Token.TokenName.AssignBitXor, Token.TokenName.BitXor);
                    break;
                case CharacterCodes.dot:
                    // . Number
                    this.advance();
                    if (isDecimalDigit(this.char))
                        token = this.scanNumber(CharacterCodes.dot);
                    else
                        token = Token.TokenName.Period;
                    break;
                case CharacterCodes.colon:
                    token = this.selectToken(Token.TokenName.Colon);
                    break;
                case CharacterCodes.semicolon:
                    token = this.selectToken(Token.TokenName.Semicolon);
                    break;
                case CharacterCodes.comma:
                    token = this.selectToken(Token.TokenName.Comma);
                    break;
                case CharacterCodes.openParen:
                    token = this.selectToken(Token.TokenName.LParen);
                    break;
                case CharacterCodes.closeParen:
                    token = this.selectToken(Token.TokenName.RParen);
                    break;
                case CharacterCodes.openBracket:
                    token = this.selectToken(Token.TokenName.LBrack);
                    break;
                case CharacterCodes.closeBracket:
                    token = this.selectToken(Token.TokenName.RBrack);
                    break;
                case CharacterCodes.openBrace:
                    token = this.selectToken(Token.TokenName.LBrace);
                    break;
                case CharacterCodes.closeBrace:
                    token = this.selectToken(Token.TokenName.RBrace);
                    break;
                case CharacterCodes.question:
                    token = this.selectToken(Token.TokenName.Conditional);
                    break;
                case CharacterCodes.tilde:
                    token = this.selectToken(Token.TokenName.BitNot);
                    break;
                default:
                    if (isIdentifierStart(this.char)) {
                        const result = this.scanIdentifierOrKeyword();
                        token = result.token;
                        m = result.m;
                        n = result.n;

                        // Special case for hexademical literals
                        if (token === Token.TokenName.Hex) {
                            // reset
                            m = 0;
                            n = 0;

                            // Special quoted hex string must follow
                            if (this.char === CharacterCodes.doubleQuote || this.char === CharacterCodes.singleQuote)
                                token = this.scanHexString();
                            else
                                token = Token.TokenName.Illegal;
                        }
                    }
                    else if (isDecimalDigit(this.char))
                        token = this.scanNumber();
                    else if (this.skipWhitespace())
                        token = Token.TokenName.Whitespace;
                    else if (this.isSourcePastEndOfInput())
                        token = Token.TokenName.EOS;
                    else
                        token = this.selectToken(Token.TokenName.Illegal);
                    break;
            }
            // Continue scanning for tokens as long as we're just skipping
            // whitespace.
        } while (token === Token.TokenName.Whitespace);
        this.nextToken.location.end = this.sourcePos;
        this.nextToken.token = token;
        this.nextToken.extendedTokenInfo = { m, n };
    }

    private scanIdentifierOrKeyword(): { token: Token.TokenName, m: number, n: number } {
        Debug.assert(isIdentifierStart(this.char));
        const literal = new LiteralScope(this, LiteralType.String);
        try {
            this.addLiteralCharAndAdvance();
            // Scan the rest of the identifier characters.
            while (isIdentifierPart(this.char)) // get full literal
                this.addLiteralCharAndAdvance();
            literal.complete();
            return Token.fromIdentifierOrKeyword(this.nextToken.literal);
        } finally {
            literal.destroy();
        }
    }

    private scanString(): Token.TokenName {
        const quote = this.char;
        this.advance();  // consume quote

        const literal = new LiteralScope(this, LiteralType.String);
        try {
            while (this.char !== quote && !this.isSourcePastEndOfInput() && !isLineTerminator(this.char)) {
                const c = this.char;
                this.advance();
                if (c === CharacterCodes.backslash) {
                    if (this.isSourcePastEndOfInput() || !this.scanEscape())
                        return Token.TokenName.Illegal;
                }
                else
                    this.addLiteralChar(c);
            }
            if (this.char !== quote)
                return Token.TokenName.Illegal;
            literal.complete();
            this.advance();  // consume quote
            return Token.TokenName.StringLiteral;
        } finally {
            literal.destroy();
        }
    }

    private scanNumber(charSeen = 0): Token.TokenName {
        let kind = NumberKind.Decimal;
        const literal = new LiteralScope(this, LiteralType.Number);
        try {
            if (charSeen === CharacterCodes.dot) {
                // we have already seen a decimal point of the float
                this.addLiteralChar(CharacterCodes.dot);
                this.scanDecimalDigits();  // we know we have at least one digit
            }
            else {
                Debug.assert(charSeen === 0);
                // if the first character is '0' we must check for octals and hex
                if (this.char === CharacterCodes._0) {
                    this.addLiteralCharAndAdvance();
                    // either 0, 0exxx, 0Exxx, 0.xxx or a hex number
                    if (this.char as number === CharacterCodes.x || this.char as number === CharacterCodes.X) {
                        // hex number
                        kind = NumberKind.Hex;
                        this.addLiteralCharAndAdvance();
                        if (!isHexDigit(this.char))
                            return Token.TokenName.Illegal; // we must have at least one hex digit after 'x'/'X'
                        while (isHexDigit(this.char))
                            this.addLiteralCharAndAdvance();
                    }
                    else if (isDecimalDigit(this.char))
                        // We do not allow octal numbers
                        return Token.TokenName.Illegal;
                }
                // Parse decimal digits and allow trailing fractional part.
                if (kind === NumberKind.Decimal) {
                    this.scanDecimalDigits();  // optional
                    if (this.char === CharacterCodes.dot) {
                        this.addLiteralCharAndAdvance();
                        this.scanDecimalDigits();  // optional
                    }
                }
            }
            // scan exponent, if any
            if (this.char === CharacterCodes.e || this.char === CharacterCodes.E) {
                Debug.assert(kind !== NumberKind.Hex, "'e'/'E' must be scanned as part of the hex number");
                if (kind !== NumberKind.Decimal)
                    return Token.TokenName.Illegal;
                // scan exponent
                this.addLiteralCharAndAdvance();
                if (this.char as number === CharacterCodes.plus || this.char as number === CharacterCodes.minus)
                    this.addLiteralCharAndAdvance();
                if (!isDecimalDigit(this.char))
                    return Token.TokenName.Illegal; // we must have at least one decimal digit after 'e'/'E'
                this.scanDecimalDigits();
            }
            // The source character immediately following a numeric literal must
            // not be an identifier start or a decimal digit; see ECMA-262
            // section 7.8.3, page 17 (note that we read only one decimal digit
            // if the value is 0).
            if (isDecimalDigit(this.char) || isIdentifierStart(this.char))
                return Token.TokenName.Illegal;
            literal.complete();
            return Token.TokenName.Number;
        } finally {
            literal.destroy();
        }
    }

    private scanDecimalDigits() {
        while (isDecimalDigit(this.char))
            this.addLiteralCharAndAdvance();
    }

    private scanHexString(): Token.TokenName {
        const quote = this.char;
        this.advance();  // consume quote
        const literal = new LiteralScope(this, LiteralType.String);
        while (this.char !== quote && !this.isSourcePastEndOfInput() && !isLineTerminator(this.char)) {
            const c = this.scanHexByte();
            if (c === undefined)
                return Token.TokenName.Illegal;
            this.addLiteralChar(c);
        }
        if (this.char !== quote)
            return Token.TokenName.Illegal;
        literal.complete();
        this.advance();  // consume quote
        return Token.TokenName.StringLiteral;
    }

    private scanEscape(): boolean {
        let c = this.char;
        this.advance();
        // Skip escaped newlines.
        if (isLineTerminator(c))
            return true;
        switch (c) {
            case CharacterCodes.singleQuote: // fall through
            case CharacterCodes.doubleQuote: // fall through
            case CharacterCodes.backslash:
                break;
            case CharacterCodes.b:
                c = CharacterCodes.backspace;
                break;
            case CharacterCodes.f:
                c = CharacterCodes.formFeed;
                break;
            case CharacterCodes.n:
                c = CharacterCodes.lineFeed;
                break;
            case CharacterCodes.r:
                c = CharacterCodes.lineFeed;
                break;
            case CharacterCodes.t:
                c = CharacterCodes.tab;
                break;
            case CharacterCodes.v:
                c = CharacterCodes.verticalTab;
                break;
            case CharacterCodes.u:
                {
                    const codepoint = this.scanUnicode();
                    if (codepoint === undefined)
                        return false;
                    this.addUnicodeAsUTF8(codepoint);
                    return true;
                }
            case CharacterCodes.x:
                c = this.scanHexByte();
                if (c === undefined)
                    return false;
                break;
        }

        this.addLiteralChar(c);
        return true;
    }

    private scanHexByte(): number | undefined {
        let x = 0;
        for (let i = 0; i < 2; i++) {
            const d = hexValue(this.char);
            if (d < 0) {
                this.rollback(i);
                return undefined;
            }
            x = x * 16 + d;
            this.advance();
        }
        return x;
    }

    private scanUnicode(): number | undefined {
        let x = 0;
        for (let i = 0; i < 4; i++) {
            const d = hexValue(this.char);
            if (d < 0) {
                this.rollback(i);
                return undefined;
            }
            x = x * 16 + d;
            this.advance();
        }
        return x;
    }

    private scanSlash(): Token.TokenName {
        const firstSlashPosition = this.sourcePos;
        this.advance();
        if (this.char === CharacterCodes.slash) {
            if (!this.advance()) /* double slash comment directly before EOS */
                return Token.TokenName.Whitespace;
            else if (this.char === CharacterCodes.slash) {
                // doxygen style /// comment
                this.nextSkippedComment.location.start = firstSlashPosition;
                const comment = this.scanSingleLineDocComment();
                this.nextSkippedComment.location.end = this.sourcePos;
                this.nextSkippedComment.token = comment;
                return Token.TokenName.Whitespace;
            }
            else
                return this.skipSingleLineComment();
        }
        else if (this.char === CharacterCodes.asterisk) {
            // doxygen style /** natspec comment
            if (!this.advance()) /* slash star comment before EOS */
                return Token.TokenName.Whitespace;
            else if (this.char === CharacterCodes.asterisk) {
                this.advance(); // consume the last '*' at /**
                this.skipWhitespaceExceptLF();

                // special case of a closed normal multiline comment
                if (!this.source.isPastEndOfInput() && this.source.get(0) === CharacterCodes.slash)
                    this.advance(); // skip the closing slash
                else { // we actually have a multiline documentation comment
                    this.nextSkippedComment.location.start = firstSlashPosition;
                    const comment = this.scanMultiLineDocComment();
                    this.nextSkippedComment.location.end = this.sourcePos;
                    this.nextSkippedComment.token = comment;
                }
                return Token.TokenName.Whitespace;
            }
            else
                return this.skipMultiLineComment();
        }
        else if (this.char === CharacterCodes.equals)
            return this.selectToken(Token.TokenName.AssignDiv);
        else
            return Token.TokenName.Div;
    }

    private scanSingleLineDocComment(): Token.TokenName {
        const literal = new LiteralScope(this, LiteralType.Comment);
        try {
            this.advance(); // consume the last '/' at ///
            this.skipWhitespaceExceptLF();
            while (!this.isSourcePastEndOfInput()) {
                if (isLineTerminator(this.char)) {
                    // check if next line is also a documentation comment
                    this.skipWhitespace();
                    if (!this.source.isPastEndOfInput(3) &&
                        this.source.get(0) === CharacterCodes.slash &&
                        this.source.get(1) === CharacterCodes.slash &&
                        this.source.get(2) === CharacterCodes.slash) {
                        this.addCommentLiteralChar(CharacterCodes.lineFeed);
                        this.char = this.source.advanceAndGet(3);
                    }
                    else
                        break; // next line is not a documentation comment, we are done

                }
                this.addCommentLiteralChar(this.char);
                this.advance();
            }
            literal.complete();
            return Token.TokenName.CommentLiteral;
        } finally {
            literal.destroy();
        }
    }

    private scanMultiLineDocComment(): Token.TokenName {
        const literal = new LiteralScope(this, LiteralType.Comment);
        try {
            let endFound = false;
            let charsAdded = false;

            while (!this.isSourcePastEndOfInput()) {
                // handle newlines in multline comments
                if (isLineTerminator(this.char)) {
                    this.skipWhitespace();
                    if (!this.source.isPastEndOfInput(1) && this.source.get(0) === CharacterCodes.asterisk && this.source.get(1) === CharacterCodes.asterisk) { // it is unknown if this leads to the end of the comment
                        this.addCommentLiteralChar(CharacterCodes.asterisk);
                        this.advance();
                    }
                    else if (!this.source.isPastEndOfInput(1) && this.source.get(0) === CharacterCodes.asterisk && this.source.get(1) !== CharacterCodes.slash) { // skip first '*' in subsequent lines
                        if (charsAdded)
                            this.addCommentLiteralChar(CharacterCodes.lineFeed);
                        this.char = this.source.advanceAndGet(2);
                    }
                    else if (!this.source.isPastEndOfInput(1) && this.source.get(0) === CharacterCodes.asterisk && this.source.get(1) === CharacterCodes.slash) { // if after newline the comment ends, don't insert the newline
                        this.char = this.source.advanceAndGet(2);
                        endFound = true;
                        break;
                    }
                    else if (charsAdded)
                        this.addCommentLiteralChar(CharacterCodes.lineFeed);
                }

                if (!this.source.isPastEndOfInput(1) && this.source.get(0) === CharacterCodes.asterisk && this.source.get(1) === CharacterCodes.slash) {
                    this.char = this.source.advanceAndGet(2);
                    endFound = true;
                    break;
                }
                this.addCommentLiteralChar(this.char);
                charsAdded = true;
                this.advance();
            }
            literal.complete();
            if (!endFound)
                return Token.TokenName.Illegal;
            else
                return Token.TokenName.CommentLiteral;
        } finally {
            literal.destroy();
        }
    }

    private addLiteralChar(c: number) {
        this.nextToken.literal.push(c);
    }

    private addLiteralCharAndAdvance() {
        this.addLiteralChar(this.char);
        this.advance();
    }

    private addCommentLiteralChar(c: number) {
        this.nextSkippedComment.literal.push(c);
    }

    // This supports codepoints between 0000 and FFFF.
    private addUnicodeAsUTF8(codepoint: number) {
        if (codepoint <= 0x7f)
            this.addLiteralChar(codepoint);
        else if (codepoint <= 0x7ff) {
            this.addLiteralChar(0xc0 | (codepoint >> 6));
            this.addLiteralChar(0x80 | (codepoint & 0x3f));
        }
        else {
            this.addLiteralChar(0xe0 | (codepoint >> 12));
            this.addLiteralChar(0x80 | ((codepoint >> 6) & 0x3f));
            this.addLiteralChar(0x80 | (codepoint & 0x3f));
        }
    }

    private advance(): boolean {
        this.char = this.source.advanceAndGet();
        return !this.source.isPastEndOfInput();
    }

    private rollback(amount: number) {
        this.char = this.source.rollback(amount);
    }

    private selectToken(tok: Token.TokenName) {
        this.advance();
        return tok;
    }

    private selectTokenAlt(_next: number, _then: Token.TokenName, _else: Token.TokenName): Token.TokenName {
        this.advance();
        if (this.char === _next)
            return this.selectToken(_then);
        else
            return _else;
    }

    private isSourcePastEndOfInput(): boolean {
        return this.source.isPastEndOfInput();
    }

    private skipWhitespace(): boolean {
        const startPosition = this.sourcePos;
        while (isWhiteSpace(this.char))
            this.advance();
        // Return whether or not we skipped any characters.
        return this.sourcePos !== startPosition;
    }

    private skipWhitespaceExceptLF(): boolean {
        const startPosition = this.sourcePos;
        while (isWhiteSpace(this.char) && !isLineTerminator(this.char))
            this.advance();
        // Return whether or not we skipped any characters.
        return this.sourcePos !== startPosition;
    }

    private skipSingleLineComment(): Token.TokenName {
        // The line terminator at the end of the line is not considered
        // to be part of the single-line comment; it is recognized
        // separately by the lexical grammar and becomes part of the
        // stream of input elements for the syntactic grammar
        while (!isLineTerminator(this.char))
            if (!this.advance()) break;

        return Token.TokenName.Whitespace;
    }

    private skipMultiLineComment(): Token.TokenName {
        this.advance();
        while (!this.isSourcePastEndOfInput()) {
            const ch = this.char;
            this.advance();

            // If we have reached the end of the multi-line comment, we
            // consume the '/' and insert a whitespace. This way all
            // multi-line comments are treated as whitespace.
            if (ch === CharacterCodes.asterisk && this.char === CharacterCodes.slash) {
                this.char = CharacterCodes.space;
                return Token.TokenName.Whitespace;
            }
        }
        // Unterminated multi-line comment.
        return Token.TokenName.Illegal;
    }
}
