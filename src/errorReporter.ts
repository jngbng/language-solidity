import { Diagnostic, DiagnosticCategory, SourceLocation } from "./types";

export class ErrorReporter {
    private _errors: Diagnostic[] = [];

    public get errors(): Diagnostic[] {
        return this._errors;
    }

    public error(type: DiagnosticCategory, description: string, location: SourceLocation) {
        this._errors.push(new Diagnostic(type, description, location));
    }

    public warning(description: string, location?: SourceLocation) {
        this.error(DiagnosticCategory.Warning, description, location);
    }

    public declarationError(description: string, location?: SourceLocation) {
        this.error(DiagnosticCategory.DeclarationError, description, location);
    }

    public parserError(description: string, location?: SourceLocation) {
        this.error(DiagnosticCategory.ParserError, description, location);
    }

    private fatalError(type: DiagnosticCategory, description: string, location: SourceLocation) {
        this.error(type, description, location);
        throw new Error("Fatal error");
    }

    public fatalParserError(description: string, location?: SourceLocation) {
        this.fatalError(DiagnosticCategory.ParserError, description, location);
    }

    public syntaxError(description: string, location?: SourceLocation) {
        this.error(DiagnosticCategory.SyntaxError, description, location);
    }

    public typeError(description: string, location?: SourceLocation) {
        this.error(DiagnosticCategory.TypeError, description, location);
    }

    public docstringParsingError(description: string) {
        this.error(DiagnosticCategory.DocstringParsingError, description, new SourceLocation());
    }

    public clear() {
        this._errors = [];
    }
}
