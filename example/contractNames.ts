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
    const source = fs.readFileSync("MetaCoin.sol").toString();
    const scanner = new Scanner(new CharStream(source));
    const reporter = new DiagnosticReporter();
    const parser = new Parser(reporter);
    const node = parser.parse(scanner);
    const collector = new ContractNameCollector(node);
    console.log(collector.contractNames);
}

main();