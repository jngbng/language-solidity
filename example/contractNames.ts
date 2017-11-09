import * as fs from "fs";

import {
    ASTVisitor,
    CharStream,
    ContractDefinition,
    DiagnosticReporter,
    Parser,
    Scanner,
    SourceUnit
} from "../src";

class ContractNameCollector extends ASTVisitor {
    public contractNames: string[] = [];

    constructor(node: SourceUnit) {
        super();
        node.accept(this);
    }

    public visitContractDefinition(node: ContractDefinition): boolean {
        this.contractNames.push(node.name);
        return true;
    }
}

function main() {
    const sourceText = fs.readFileSync("MetaCoin.sol").toString();
    const reporter = new DiagnosticReporter();
    const parser = new Parser(reporter);
    const node = parser.parseSourceFile("MetaCoin.sol", sourceText);
    const collector = new ContractNameCollector(node);
    console.log(collector.contractNames);
}

main();