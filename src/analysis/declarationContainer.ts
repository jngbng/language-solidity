import {
    ASTNode,
    Declaration,
    EventDefinition,
    FunctionDefinition,
    VariableDeclaration
} from "../compiler/ast";
import { Debug, contains, createMap, first } from "../compiler/core";
import { Map, ReadonlyMap } from "../compiler/types";

/**
 * Container that stores mappings between names and declarations. It also contains a link to the
 * enclosing scope.
 */
export class DeclarationContainer {
    private _declarations: Map<Declaration[]> = createMap();
    private invisibleDeclarations: Map<Declaration[]> = createMap();

    constructor(
        public readonly enclosingNode?: ASTNode,
        public readonly enclosingContainer?: DeclarationContainer) {
    }

    public get declarations(): ReadonlyMap<Declaration[]> {
        return this._declarations;
    }

    /// Registers the declaration in the scope unless its name is already declared or the name is empty.
    /// @param _name the name to register, if nullptr the intrinsic name of @a _declaration is used.
    /// @param _invisible if true, registers the declaration, reports name clashes but does not return it in @a resolveName
    /// @param _update if true, replaces a potential declaration that is already present
    /// @returns false if the name was already declared.
    public registerDeclaration(declaration: Declaration, name: string = "", invisible = false, update = false): boolean {
        if (name === "")
            name = declaration.name;
        if (name === "")
            return true;

        if (update) {
            Debug.assert(!(declaration instanceof FunctionDefinition),
                "Attempt to update function definition.");
            this._declarations.delete(name);
            this.invisibleDeclarations.delete(name);
        }
        else if (this.conflictingDeclaration(declaration, name))
            return false;

        const decls = invisible ? this.invisibleDeclarations.get(name) : this._declarations.get(name);
        if (!contains(decls, declaration))
            decls.push(declaration);
        return true;
    }

    public resolveName(name: string, recursive = false): Declaration[] {
        Debug.assert(name !== "", "Attempt to resolve empty name.");
        const result = this._declarations.get(name);
        if (result)
            return result;
        if (recursive && this.enclosingContainer)
            return this.enclosingContainer.resolveName(name, true);
        return [];
    }

    /// @returns whether declaration is valid, and if not also returns previous declaration.
    public conflictingDeclaration(declaration: Declaration, name = ""): Declaration | undefined {
        if (name === "")
            name = declaration.name;
        Debug.assert(name !== "");
        const declarations: Declaration[] = [];
        if (this._declarations.has(name))
            declarations.push(...this._declarations.get(name));
        if (this.invisibleDeclarations.has(name))
            declarations.push(...this.invisibleDeclarations.get(name));

        if (declaration instanceof FunctionDefinition || declaration instanceof EventDefinition) {
            // check that all other declarations with the same name are functions or a public state variable or events.
            // And then check that the signatures are different.
            for (const decl of declarations) {
                if (decl instanceof VariableDeclaration) {
                    if (decl.isStateVariable() && !decl.isConstant() && decl.isPublic())
                        continue;
                    return decl;
                }
                if (declaration instanceof FunctionDefinition && !(decl instanceof FunctionDefinition))
                    return decl;
                if (declaration instanceof EventDefinition && !(decl instanceof EventDefinition))
                    return decl;
                // Or, continue.
            }
        }
        else if (declarations.length === 1 && first(declarations) === declaration)
            return undefined;
        else if (declarations.length !== 0)
            return first(declarations);

        return undefined;
    }
}
