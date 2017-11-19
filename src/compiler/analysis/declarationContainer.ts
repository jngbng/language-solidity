import {
    ASTNode,
    Declaration,
    isEventDefinition,
    isFunctionDefinition,
    isVariableDeclaration,
} from "../ast/ast";
import { Debug, MultiMap, contains, createMultiMap, first } from "../core";

/**
 * Container that stores mappings between names and declarations. It also contains a link to the
 * enclosing scope.
 */
export class DeclarationContainer {
    private _declarations: MultiMap<Declaration> = createMultiMap();
    private invisibleDeclarations: MultiMap<Declaration> = createMultiMap();

    constructor(
        public readonly enclosingNode?: ASTNode,
        public readonly enclosingContainer?: DeclarationContainer) {
    }

    public get declarations(): MultiMap<Declaration> {
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
            Debug.assert(!isFunctionDefinition(declaration),
                "Attempt to update function definition.");
            this._declarations.delete(name);
            this.invisibleDeclarations.delete(name);
        }
        else if (this.conflictingDeclaration(declaration, name))
            return false;

        if (invisible) {
            const decls = this.invisibleDeclarations.get(name);
            if (!contains(decls, declaration))
                this.invisibleDeclarations.add(name, declaration);
        } else {
            const decls = this._declarations.get(name);
            if (!contains(decls, declaration))
                this._declarations.add(name, declaration);
        }
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

        if (isFunctionDefinition(declaration) || isEventDefinition(declaration)) {
            // check that all other declarations with the same name are functions or a public state variable or events.
            // And then check that the signatures are different.
            for (const decl of declarations) {
                if (isVariableDeclaration(decl)) {
                    if (decl.isStateVariable() && !decl.isConstant() && decl.isPublic())
                        continue;
                    return decl;
                }
                if (isFunctionDefinition(declaration) && !isFunctionDefinition(decl))
                    return decl;
                if (isEventDefinition(declaration) && !isEventDefinition(decl))
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
