import { ErrorReporter } from "./errorReporter";
import { Scanner, tokenToString } from "./scanner";
import { SourceLocation, TokenName, isElementaryTypeName, isReservedKeyword } from "./types";

export class ParserBase {
    private errorReporter: ErrorReporter;
    private scanner: Scanner;

    constructor(errorReporter: ErrorReporter) {
        this.errorReporter = errorReporter;
        this.scanner = new Scanner();
    }

    public get sourceName() {
        return this.scanner.sourceName;
    }

    /// Start position of the current token
    protected get position(): number {
        return this.scanner.currentLocation.start;
    }

    /// End position of the current token
    protected get endPosition(): number {
        return this.scanner.currentLocation.end;
    }

    protected get currentToken(): TokenName {
        return this.scanner.currentToken;
    }

    protected get currentLiteral(): string {
        return this.scanner.currentLiteral;
    }

    protected peekNextToken(): TokenName {
        return this.scanner.peekNextToken();
    }

    protected advance(): TokenName {
        return this.scanner.next();
    }

    protected expectToken(value: TokenName) {
        const tok = this.scanner.currentToken;
        if (tok !== value) {
            if (isReservedKeyword(tok)) {
                this.fatalParserError(
                    `Expected token ${tokenToString(value)} got reserved keyword '${tokenToString(tok)}'`);
            }
            else if (isElementaryTypeName(tok)) { // for the sake of accuracy in reporting
                const elemTypeName = this.scanner.currentElementaryTypeNameToken;
                this.fatalParserError(
                    `Expected token ${tokenToString(value)} got '${elemTypeName}'`);
            }
            else
                this.fatalParserError(
                    `Expected token ${tokenToString(value)} got '${tokenToString(this.scanner.currentToken)}'`);
        }
        this.scanner.next();
    }

    protected fatalParserError(description: string) {
        this.errorReporter.fatalParserError(
            description, new SourceLocation(this.position, this.position, this.sourceName));
    }

    protected parserError(description: string) {
        this.errorReporter.parserError(
            description, new SourceLocation(this.position, this.position, this.sourceName));
    }
}
