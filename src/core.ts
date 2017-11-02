import { Map, MapCtr, MapLike, hasOwnProperty } from "./types";
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

export function createMapFromTemplate<T>(template?: MapLike<T>): Map<T> {
    const map: Map<T> = new MapCtr<T>();

    // Copies keys/values from template. Note that for..in will not throw if
    // template is undefined, and instead will just exit the loop.
    for (const key in template) {
        if (hasOwnProperty.call(template, key)) {
            map.set(key, template[key]);
        }
    }

    return map;
}

/**
 * Iterates through `array` by index and performs the callback on each element of array until the callback
 * returns a falsey value, then returns false.
 * If no such value is found, the callback is applied to each element of array and `true` is returned.
 */
export function every<T>(array: ReadonlyArray<T>, callback: (element: T, index: number) => boolean, options: { from?: number, to?: number } = {}): boolean {
    if (array) {
        const { from = 0, to = array.length } = options;
        for (let i = from; i < to; i++) {
            if (!callback(array[i], i)) {
                return false;
            }
        }
    }

    return true;
}

/** Works like Array.prototype.findIndex, returning `-1` if no element satisfying the predicate is found. */
export function findIndex<T>(array: ReadonlyArray<T>, predicate: (element: T, index: number) => boolean, options: { from?: number, to?: number } = {}): number {
    const { from = 0, to = array.length } = options;

    for (let i = from; i < to; i++) {
        if (predicate(array[i], i)) {
            return i;
        }
    }
    return -1;
}


export function clone<T>(object: T): T {
    const result: any = {};
    for (const id in object) {
        if (hasOwnProperty.call(object, id)) {
            result[id] = (<any>object)[id];
        }
    }
    return result;
}

export function applyMixins(derivedCtor: any, baseCtors: any[]) {
    baseCtors.forEach(baseCtor => {
        Object.getOwnPropertyNames(baseCtor.prototype).forEach(name => {
            derivedCtor.prototype[name] = baseCtor.prototype[name];
        });
    });
}
