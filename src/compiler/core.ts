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

export interface MultiMap<T> extends Map<T[]> {
    /**
     * Adds the value to an array of values associated with the key, and returns the array.
     * Creates the array if it does not already exist.
     */
    add(key: string, value: T): T[];
    /**
     * Removes a value from an array of values associated with the key.
     * Does not preserve the order of those values.
     * Does nothing if `key` is not in `map`, or `value` is not in `map[key]`.
     */
    remove(key: string, value: T): void;
}

/** Create a new map. If a template object is provided, the map will copy entries from it. */
export function createMap<T>(): Map<T> {
    return new MapCtr<T>();
}

export function createMultiMap<T>(): MultiMap<T> {
    const map = createMap<T[]>() as MultiMap<T>;
    map.add = multiMapAdd;
    map.remove = multiMapRemove;
    return map;
}
function multiMapAdd<T>(this: MultiMap<T>, key: string, value: T) {
    let values = this.get(key);
    if (values) {
        values.push(value);
    }
    else {
        this.set(key, values = [value]);
    }
    return values;

}
function multiMapRemove<T>(this: MultiMap<T>, key: string, value: T) {
    const values = this.get(key);
    if (values) {
        unorderedRemoveItem(values, value);
        if (!values.length) {
            this.delete(key);
        }
    }
}

/** Remove the *first* occurrence of `item` from the array. */
export function unorderedRemoveItem<T>(array: T[], item: T): void {
    unorderedRemoveFirstItemWhere(array, element => element === item);
}

/** Remove the *first* element satisfying `predicate`. */
function unorderedRemoveFirstItemWhere<T>(array: T[], predicate: (element: T) => boolean): void {
    for (let i = 0; i < array.length; i++) {
        if (predicate(array[i])) {
            unorderedRemoveItemAt(array, i);
            break;
        }
    }
}

export function unorderedRemoveItemAt<T>(array: T[], index: number): void {
    // Fill in the "hole" left at `index`.
    array[index] = array[array.length - 1];
    array.pop();
}

/**
 * Returns the first element of an array if non-empty, `undefined` otherwise.
 */
export function firstOrUndefined<T>(array: ReadonlyArray<T>): T | undefined {
    return elementAt(array, 0);
}

export function first<T>(array: ReadonlyArray<T>): T {
    Debug.assert(array.length !== 0);
    return array[0];
}

/**
 * Returns the last element of an array if non-empty, `undefined` otherwise.
 */
export function lastOrUndefined<T>(array: ReadonlyArray<T>): T | undefined {
    return elementAt(array, -1);
}

export function last<T>(array: ReadonlyArray<T>): T {
    Debug.assert(array.length !== 0);
    return array[array.length - 1];
}


/**
 * Returns the element at a specific offset in an array if non-empty, `undefined` otherwise.
 * A negative offset indicates the element should be retrieved from the end of the array.
 */
export function elementAt<T>(array: ReadonlyArray<T> | undefined, offset: number): T | undefined {
    if (array) {
        offset = toOffset(array, offset);
        if (offset < array.length) {
            return array[offset];
        }
    }
    return undefined;
}

/**
 * Gets the actual offset into an array for a relative offset. Negative offsets indicate a
 * position offset from the end of the array.
 */
function toOffset(array: ReadonlyArray<any>, offset: number) {
    return offset < 0 ? array.length + offset : offset;
}

/**
 * Tests whether a value is an array.
 */
export function isArray(value: any): value is ReadonlyArray<any> {
    return Array.isArray ? Array.isArray(value) : value instanceof Array;
}

/**
 * Tests whether a value is string
 */
export function isString(text: any): text is string {
    return typeof text === "string";
}

/** Do nothing and return false */
export function returnFalse(): false { return false; }

/** Do nothing and return true */
export function returnTrue(): true { return true; }
