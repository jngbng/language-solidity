import { CharacterCodes, Map, MapCtr, MapLike, Path, hasOwnProperty } from "./types";
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

export function contains<T>(array: ReadonlyArray<T>, value: T): boolean {
    if (array) {
        for (const v of array) {
            if (v === value) {
                return true;
            }
        }
    }
    return false;
}

export function removeWhere<T>(array: T[], f: (x: T) => boolean): boolean {
    let outIndex = 0;
    for (const item of array) {
        if (!f(item)) {
            array[outIndex] = item;
            outIndex++;
        }
    }
    if (outIndex !== array.length) {
        array.length = outIndex;
        return true;
    }
    return false;
}

export function arrayIsEqualTo<T>(array1: ReadonlyArray<T>, array2: ReadonlyArray<T>, equaler?: (a: T, b: T) => boolean): boolean {
    if (!array1 || !array2) {
        return array1 === array2;
    }

    if (array1.length !== array2.length) {
        return false;
    }

    for (let i = 0; i < array1.length; i++) {
        const equals = equaler ? equaler(array1[i], array2[i]) : array1[i] === array2[i];
        if (!equals) {
            return false;
        }
    }

    return true;
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

export function createGetCanonicalFileName(useCaseSensitiveFileNames: boolean): (fileName: string) => string {
    return useCaseSensitiveFileNames
        ? ((fileName) => fileName)
        : ((fileName) => fileName.toLowerCase());
}

export function isRootedDiskPath(path: string) {
    return path && getRootLength(path) !== 0;
}

/**
 * Returns length of path root (i.e. length of "/", "x:/", "//server/share/, file:///user/files")
 */
export function getRootLength(path: string): number {
    if (path.charCodeAt(0) === CharacterCodes.slash) {
        if (path.charCodeAt(1) !== CharacterCodes.slash) return 1;
        const p1 = path.indexOf("/", 2);
        if (p1 < 0) return 2;
        const p2 = path.indexOf("/", p1 + 1);
        if (p2 < 0) return p1 + 1;
        return p2 + 1;
    }
    if (path.charCodeAt(1) === CharacterCodes.colon) {
        if (path.charCodeAt(2) === CharacterCodes.slash) return 3;
        return 2;
    }
    // Per RFC 1738 'file' URI schema has the shape file://<host>/<path>
    // if <host> is omitted then it is assumed that host value is 'localhost',
    // however slash after the omitted <host> is not removed.
    // file:///folder1/file1 - this is a correct URI
    // file://folder2/file2 - this is an incorrect URI
    if (path.lastIndexOf("file:///", 0) === 0) {
        return "file:///".length;
    }
    const idx = path.indexOf("://");
    if (idx !== -1) {
        return idx + "://".length;
    }
    return 0;
}

export function toPath(fileName: string, basePath: string, getCanonicalFileName: (path: string) => string): Path {
    const nonCanonicalizedPath = isRootedDiskPath(fileName)
        ? normalizePath(fileName)
        : getNormalizedAbsolutePath(fileName, basePath);
    return <Path>getCanonicalFileName(nonCanonicalizedPath);
}

export function normalizePath(path: string): string {
    return normalizePathAndParts(path).path;
}

export function normalizePathAndParts(path: string): { path: string, parts: string[] } {
    path = normalizeSlashes(path);
    const rootLength = getRootLength(path);
    const root = path.substr(0, rootLength);
    const parts = getNormalizedParts(path, rootLength);
    if (parts.length) {
        const joinedParts = root + parts.join(directorySeparator);
        return { path: pathEndsWithDirectorySeparator(path) ? joinedParts + directorySeparator : joinedParts, parts };
    }
    else {
        return { path: root, parts };
    }
}

export function normalizeSlashes(path: string): string {
    return path.replace(/\\/g, "/");
}

/**
 * Internally, we represent paths as strings with '/' as the directory separator.
 * When we make system calls (eg: LanguageServiceHost.getDirectory()),
 * we expect the host to correctly handle paths in our specified format.
 */
export const directorySeparator = "/";
const directorySeparatorCharCode = CharacterCodes.slash;
function getNormalizedParts(normalizedSlashedPath: string, rootLength: number): string[] {
    const parts = normalizedSlashedPath.substr(rootLength).split(directorySeparator);
    const normalized: string[] = [];
    for (const part of parts) {
        if (part !== ".") {
            if (part === ".." && normalized.length > 0 && lastOrUndefined(normalized) !== "..") {
                normalized.pop();
            }
            else {
                // A part may be an empty string (which is 'falsy') if the path had consecutive slashes,
                // e.g. "path//file.ts".  Drop these before re-joining the parts.
                if (part) {
                    normalized.push(part);
                }
            }
        }
    }

    return normalized;
}

export function getNormalizedAbsolutePath(fileName: string, currentDirectory: string) {
    return getNormalizedPathFromPathComponents(getNormalizedPathComponents(fileName, currentDirectory));
}

export function getNormalizedPathFromPathComponents(pathComponents: ReadonlyArray<string>) {
    if (pathComponents && pathComponents.length) {
        return pathComponents[0] + pathComponents.slice(1).join(directorySeparator);
    }
}

export function getNormalizedPathComponents(path: string, currentDirectory: string) {
    path = normalizeSlashes(path);
    let rootLength = getRootLength(path);
    if (rootLength === 0) {
        // If the path is not rooted it is relative to current directory
        path = combinePaths(normalizeSlashes(currentDirectory), path);
        rootLength = getRootLength(path);
    }

    return normalizedPathComponents(path, rootLength);
}

export function combinePaths(path1: string, path2: string) {
    if (!(path1 && path1.length)) return path2;
    if (!(path2 && path2.length)) return path1;
    if (getRootLength(path2) !== 0) return path2;
    if (path1.charAt(path1.length - 1) === directorySeparator) return path1 + path2;
    return path1 + directorySeparator + path2;
}

function normalizedPathComponents(path: string, rootLength: number) {
    const normalizedParts = getNormalizedParts(path, rootLength);
    return [path.substr(0, rootLength)].concat(normalizedParts);
}

/** A path ending with '/' refers to a directory only, never a file. */
export function pathEndsWithDirectorySeparator(path: string): boolean {
    return path.charCodeAt(path.length - 1) === directorySeparatorCharCode;
}
