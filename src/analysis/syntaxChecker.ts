import * as semver from "semver";

import {
    Break,
    Continue,
    ForStatement,
    FunctionDefinition,
    FunctionTypeName,
    PlaceholderStatement,
    PragmaDirective,
    SourceUnit,
    Throw,
    UnaryOperation,
    WhileStatement
} from "../compiler/ast";
import { ASTNode, ModifierDefinition, visibilityToString } from "../compiler/ast";
import { ASTVisitor } from "../compiler/astVisitor";
import { Debug } from "../compiler/core";
import { DiagnosticReporter } from "../compiler/diagnosticReporter";
import { ExperimentalFeature, experimentalFeatureNames } from "../compiler/experimentalFeatures";
import { TokenName } from "../compiler/token";
import { containsOnlyWarnings } from "../compiler/types";
import { versionString } from "../interface/version";

/**
 * The module that performs syntax analysis on the AST:
 *  - whether continue/break is in a for/while loop.
 *  - whether a modifier contains at least one '_'
 *  - issues deprecation warnings for unary '+'
 *  - issues deprecation warning for throw
 */
export class SyntaxChecker extends ASTVisitor {
    /// Flag that indicates whether a function modifier actually contains '_'.
    private placeholderFound = false;

    /// Flag that indicates whether some version pragma was present.
    private versionPragmaFound = false;

    private inLoopDepth = 0;

    private sourceUnit?: SourceUnit;

    /// @param _errors the reference to the list of errors and warnings to add them found during type checking.
    constructor(private diagnosticReporter: DiagnosticReporter) {
        super();
    }

    public checkSyntax(astRoot: ASTNode): boolean {
        astRoot.accept(this);
        return containsOnlyWarnings(this.diagnosticReporter.diagnostics);
    }

    public visitSourceUnit(sourceUnit: SourceUnit): boolean {
        this.versionPragmaFound = false;
        this.sourceUnit = sourceUnit;
        return true;
    }
    public endVisitSourceUnit(_sourceUnit: SourceUnit) {
        if (!this.versionPragmaFound) {
            let errorString = "Source file does not specify required compiler version!";
            const recommendedVersion = semver.parse(versionString);
            if (recommendedVersion.prerelease.length !== 0)
                errorString +=
                    "Consider adding \"pragma solidity ^" +
                    recommendedVersion.major +
                    "." +
                    recommendedVersion.minor +
                    "." +
                    recommendedVersion.patch +
                    ";\"";

            this.diagnosticReporter.warning(errorString, this.sourceUnit.location);
        }
        this.sourceUnit = undefined;
    }

    public visitPragmaDirective(pragma: PragmaDirective): boolean {
        Debug.assert(pragma.tokens.length !== 0);
        Debug.assert(pragma.tokens.length === pragma.literals.length);
        if (pragma.tokens[0] !== TokenName.Identifier)
            this.diagnosticReporter.syntaxError(
                "Invalid pragma \"" + pragma.literals[0] + "\"", pragma.location);
        else if (pragma.literals[0] === "experimental") {
            Debug.assert(!!this.sourceUnit);
            const literals = pragma.literals.slice(1);
            if (literals.length === 0)
                this.diagnosticReporter.syntaxError(
                    "Experimental feature name is missing.", pragma.location);
            else if (literals.length > 1)
                this.diagnosticReporter.syntaxError(
                    "Stray arguments.", pragma.location);
            else {
                const literal = literals[0];
                if (literal === "")
                    this.diagnosticReporter.syntaxError(
                        "Empty experimental feature name is invalid.", pragma.location);
                else if (!experimentalFeatureNames.has(literal))
                    this.diagnosticReporter.syntaxError(
                        "Unsupported experimental feature name.", pragma.location);
                else if (this.sourceUnit.annotation.experimentalFeatures.has(experimentalFeatureNames.get(literal)))
                    this.diagnosticReporter.syntaxError(
                        "Duplicate experimental feature name.", pragma.location);
                else {
                    this.sourceUnit.annotation.experimentalFeatures.add(experimentalFeatureNames.get(literal));
                    this.diagnosticReporter.warning(
                        "Experimental features are turned on. Do not use experimental features on live deployments.", pragma.location);
                }
            }
        }
        else if (pragma.literals[0] === "solidity") {
            const literals = pragma.literals.slice(1);
            const currentVersion = semver.parse(versionString);
            if (!semver.satisfies(currentVersion, literals.join(""))) {
                this.diagnosticReporter.syntaxError(
                    "Source file requires different compiler version (current compiler is " +
                    versionString + " - note that nightly builds are considered to be " +
                    "strictly less than the released version", pragma.location);
            }
            this.versionPragmaFound = true;
        }
        else
            this.diagnosticReporter.syntaxError(
                "Unknown pragma \"" + pragma.literals[0] + "\"", pragma.location);
        return true;
    }

    public visitModifierDefinition(_modifier: ModifierDefinition): boolean {
        this.placeholderFound = false;
        return true;
    }
    public endVisitModifierDefinition(modifier: ModifierDefinition) {
        if (!this.placeholderFound)
            this.diagnosticReporter.syntaxError(
                "Modifier body does not contain '_'.", modifier.body.location);
        this.placeholderFound = false;
    }

    public visitWhileStatement(_node: WhileStatement): boolean {
        this.inLoopDepth++;
        return true;
    }
    public endVisitWhileStatement(_node: WhileStatement) {
        this.inLoopDepth--;
    }
    public visitForStatement(_node: ForStatement): boolean {
        this.inLoopDepth++;
        return true;
    }
    public endVisitForStatement(_node: ForStatement) {
        this.inLoopDepth--;
    }

    public visitContinue(continueStatement: Continue): boolean {
        if (this.inLoopDepth <= 0)
            // we're not in a for/while loop, report syntax error
            this.diagnosticReporter.syntaxError(
                "\"continue\" has to be in a \"for\" or \"while\" loop.", continueStatement.location);
        return true;
    }
    public visitBreak(breakStatement: Break): boolean {
        if (this.inLoopDepth <= 0)
            // we're not in a for/while loop, report syntax error
            this.diagnosticReporter.syntaxError(
                "\"break\" has to be in a \"for\" or \"while\" loop.", breakStatement.location);
        return true;
    }

    public visitThrow(throwStatement: Throw): boolean {
        this.diagnosticReporter.warning(
            "\"throw\" is deprecated in favour of \"revert()\", \"require()\" and \"assert()\".",
            throwStatement.location);

        return true;
    }

    public visitUnaryOperation(operation: UnaryOperation): boolean {
        const v050 = this.sourceUnit.annotation.experimentalFeatures.has(ExperimentalFeature.V050);

        if (operation.operator === TokenName.Add) {
            if (v050)
                this.diagnosticReporter.syntaxError("Use of unary + is deprecated.", operation.location);
            else
                this.diagnosticReporter.warning("Use of unary + is deprecated.", operation.location);
        }
        return true;
    }

    public visitPlaceholderStatement(_node: PlaceholderStatement): boolean {
        this.placeholderFound = true;
        return true;
    }

    public visitFunctionDefinition(fun: FunctionDefinition): boolean {
        if (fun.noVisibilitySpecified())
            this.diagnosticReporter.warning(
                "No visibility specified. Defaulting to \"" + visibilityToString(fun.visibility) + "\".", fun.location);
        return true;
    }

    public visitFunctionTypeName(_node: FunctionTypeName): boolean {
        for (const decl of _node.parameterTypeList.parameters) {
            if (decl.name !== "")
                this.diagnosticReporter.warning(
                    "Naming function type parameters is deprecated.", decl.location);
        }
        for (const decl of _node.returnParameterTypeList.parameters) {
            if (decl.name !== "")
                this.diagnosticReporter.warning(
                    "Naming function type return parameters is deprecated.", decl.location);
        }

        return true;
    }
}
