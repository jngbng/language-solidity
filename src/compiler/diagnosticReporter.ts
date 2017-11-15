import { Diagnostic, DiagnosticCategory, SecondarySourceLocation, SourceLocation } from "./types";

export class DiagnosticReporter {
    private _diagnostics: Diagnostic[] = [];

    public get diagnostics(): Diagnostic[] {
        return this._diagnostics;
    }

    public error(type: DiagnosticCategory, description: string, location: SourceLocation, secondarySourceLocation?: SecondarySourceLocation) {
        this._diagnostics.push(new Diagnostic(type, description, location, secondarySourceLocation));
    }
    public warning(description: string, location?: SourceLocation, secondarySourceLocation?: SecondarySourceLocation) {
        this.error(DiagnosticCategory.Warning, description, location, secondarySourceLocation);
    }

    public declarationError(description: string, location?: SourceLocation, secondarySourceLocation?: SecondarySourceLocation) {
        this.error(DiagnosticCategory.DeclarationError, description, location, secondarySourceLocation);
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

    public fatalTypeError(description: string, location?: SourceLocation) {
        this.fatalError(DiagnosticCategory.TypeError, description, location);
    }

    public fatalDeclarationError(description: string, location?: SourceLocation) {
        this.fatalError(DiagnosticCategory.DeclarationError, description, location);
    }

    public syntaxError(description: string, location?: SourceLocation) {
        this.error(DiagnosticCategory.SyntaxError, description, location);
    }

    public typeError(description: string, location?: SourceLocation, secondarySourceLocation?: SecondarySourceLocation) {
        this.error(DiagnosticCategory.TypeError, description, location, secondarySourceLocation);
    }

    public docstringParsingError(description: string) {
        this.error(DiagnosticCategory.DocstringParsingError, description, new SourceLocation());
    }

    public clear() {
        this._diagnostics = [];
    }
}
