import { Program } from "../compiler/ast";
import { CompilerOptions, Diagnostic } from "../compiler/types";

//
// Public services of a language service instance associated
// with a language service host instance
//
export interface LanguageService {
    getSyntacticDiagnostics(fileName: string): Diagnostic[];

    getProgram(): Program;
}

export interface HostCancellationToken {
    isCancellationRequested(): boolean;
}

//
// Public interface of the host of a language service instance.
//
export interface LanguageServiceHost {
    getCompilationSettings(): CompilerOptions;
    getNewLine?(): string;
    getProjectVersion?(): string;
    getScriptFileNames(): string[];
    getScriptVersion(fileName: string): string;
    getLocalizedDiagnosticMessages?(): any;
    getCancellationToken?(): HostCancellationToken;
    getCurrentDirectory(): string;
    log?(s: string): void;
    trace?(s: string): void;
    error?(s: string): void;
    useCaseSensitiveFileNames?(): boolean;

    /*
     * LS host can optionally implement these methods to support completions for module specifiers.
     * Without these methods, only completions for ambient modules will be provided.
     */
    readDirectory?(path: string, extensions?: ReadonlyArray<string>, exclude?: ReadonlyArray<string>, include?: ReadonlyArray<string>, depth?: number): string[];
    readFile?(path: string, encoding?: string): string | undefined;
    fileExists?(path: string): boolean;

    directoryExists?(directoryName: string): boolean;

    /*
     * getDirectories is also required for full import and type reference completions. Without it defined, certain
     * completions will not be provided
     */
    getDirectories?(directoryName: string): string[];
}
