import { SourceLocation } from "./types";

class IDDispenser {
    private id = 0;

    public next(): number { return ++this.id; }
    public reset() {
        this.id = 0;
    }
}

const idDispenser = new IDDispenser();

export class ASTNode {
    private _id: number;

    private _location: SourceLocation;

    public get id() {
        return this._id;
    }

    public get location() {
        return this._location;
    }

    constructor(location: SourceLocation) {
        this._id = idDispenser.next();
        this._location = location;
    }
}
