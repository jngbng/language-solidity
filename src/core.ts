export const enum AssertionLevel {
    None = 0,
    Normal = 1,
    Aggressive = 2,
    VeryAggressive = 3,
}

export namespace Debug {
    export let currentAssertionLevel = AssertionLevel.None;
    export let isDebugging = false;

    export function shouldAssert(level: AssertionLevel): boolean {
        return currentAssertionLevel >= level;
    }

    export function assert(expression: boolean, message?: string, verboseDebugInfo?: string | (() => string), stackCrawlMark?: Function): void {
        if (!expression) {
            if (verboseDebugInfo) {
                message += "\r\nVerbose Debug Information: " + (typeof verboseDebugInfo === "string" ? verboseDebugInfo : verboseDebugInfo());
            }
            fail(message ? "False expression: " + message : "False expression.", stackCrawlMark || assert);
        }
    }

    export function assertEqual<T>(a: T, b: T, msg?: string, msg2?: string): void {
        if (a !== b) {
            const message = msg ? msg2 ? `${msg} ${msg2}` : msg : "";
            fail(`Expected ${a} === ${b}. ${message}`);
        }
    }

    export function assertLessThan(a: number, b: number, msg?: string): void {
        if (a >= b) {
            fail(`Expected ${a} < ${b}. ${msg || ""}`);
        }
    }

    export function assertLessThanOrEqual(a: number, b: number): void {
        if (a > b) {
            fail(`Expected ${a} <= ${b}`);
        }
    }

    export function assertGreaterThanOrEqual(a: number, b: number): void {
        if (a < b) {
            fail(`Expected ${a} >= ${b}`);
        }
    }

    export function fail(message?: string, stackCrawlMark?: Function): never {
        debugger;
        const e = new Error(message ? `Debug Failure. ${message}` : "Debug Failure.");
        if ((<any>Error).captureStackTrace) {
            (<any>Error).captureStackTrace(e, stackCrawlMark || fail);
        }
        throw e;
    }

    export function assertNever(member: never, message?: string, stackCrawlMark?: Function): never {
        return fail(message || `Illegal value: ${member}`, stackCrawlMark || assertNever);
    }

    export function getFunctionName(func: Function) {
        if (typeof func !== "function") {
            return "";
        }
        else if (func.hasOwnProperty("name")) {
            return (<any>func).name;
        }
        else {
            const text = Function.prototype.toString.call(func);
            const match = /^function\s+([\w\$]+)\s*\(/.exec(text);
            return match ? match[1] : "";
        }
    }
}
