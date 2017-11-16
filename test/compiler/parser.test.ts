import { ContractDefinition, FunctionDefinition } from "../../src/compiler/ast/ast";
import { DiagnosticReporter } from "../../src/compiler/interface/diagnosticReporter";
import { Parser } from "../../src/compiler/parsing/parser";
import { CharStream, Scanner } from "../../src/compiler/parsing/scanner";
import {
    Diagnostic,
    DiagnosticCategory,
    containsDiagnosticOfCategory,
    containsOnlyWarnings
} from "../../src/compiler/types";

function parseText(source: string): { node?: ContractDefinition, diagnostics: Diagnostic[] } {
    const diagnosticReporter = new DiagnosticReporter();
    const sourceUnit = new Parser(diagnosticReporter).parseSourceFile("", source);
    if (!sourceUnit)
        return { diagnostics: diagnosticReporter.diagnostics };

    for (const node of sourceUnit.nodes) {
        if (node instanceof ContractDefinition)
            return { node, diagnostics: diagnosticReporter.diagnostics };
    }
    fail("No contract found in source.");
    return { diagnostics: diagnosticReporter.diagnostics };
}

function successParse(source: string): boolean {
    let diagnostics: Diagnostic[];
    try {
        const { node: sourceUnit, diagnostics: errors } = parseText(source);
        diagnostics = errors;
        if (!sourceUnit)
            return false;
    }
    catch (err) {
        if (containsDiagnosticOfCategory(diagnostics, DiagnosticCategory.ParserError))
            return false;
    }
    if (containsDiagnosticOfCategory(diagnostics, DiagnosticCategory.ParserError))
        return false;

    expect(containsOnlyWarnings(diagnostics)).toBeTruthy();
    return true;
}

function getDiagnostic(source: string): Diagnostic {
    let diagnostics: Diagnostic[];
    try {
        const { diagnostics: errors } = parseText(source);
        diagnostics = errors;
    }
    catch (err) {
        // no-op
    }
    const error = containsDiagnosticOfCategory(diagnostics, DiagnosticCategory.ParserError);
    expect(error).toBeTruthy();
    return error;
}

function checkFunctionNatspec(fun: FunctionDefinition, expectedDoc: string) {
    const doc = fun.documentation();
    expect(doc).toBeTruthy(); // Function does not have Natspec Doc as expected
    expect(doc).toBe(expectedDoc);
}

function checkParseError(source: string, substring: string) {
    const diagnostic = getDiagnostic(source);
    expect(diagnostic.desription).toMatch(substring);
}

test("smoke_test", () => {
    const text = `
    contract test {
        uint256 stateVariable1;
    }
    `;
    expect(successParse(text)).toBeTruthy();
});

test("missing_variable_name_in_declaration", () => {
    const text = `
    contract test {
        uint256;
    }
    `;
    checkParseError(text, "Expected identifier");
});

test("empty_function", () => {
    const text = `
    contract test {
        uint256 stateVar;
        function functionName(bytes20 arg1, address addr) constant
        returns(int id)
        { }
    }
	`;
    expect(successParse(text)).toBeTruthy();
});

test("no_function_params", () => {
    const text = `
    contract test {
        uint256 stateVar;
        function functionName() { }
    }
	`;
    expect(successParse(text)).toBeTruthy();
});

test("single_function_param", () => {
    const text = `
		contract test {
			uint256 stateVar;
			function functionName(bytes32 input) returns (bytes32 out) {}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("single_function_param_trailing_comma", () => {
    const text = `
		contract test {
			function(uint a,) {}
		}
	`;
    checkParseError(text, "Unexpected trailing comma in parameter list.");
});

test("single_return_param_trailing_comma", () => {
    const text = `
		contract test {
			function() returns (uint a,) {}
		}
	`;
    checkParseError(text, "Unexpected trailing comma in parameter list.");
});

test("single_modifier_arg_trailing_comma", () => {
    const text = `
		contract test {
			modifier modTest(uint a,) { _; }
			function(uint a) {}
		}
	`;
    checkParseError(text, "Unexpected trailing comma in parameter list.");
});

test("single_event_arg_trailing_comma", () => {
    const text = `
		contract test {
			event Test(uint a,);
			function(uint a) {}
		}
	`;
    checkParseError(text, "Unexpected trailing comma in parameter list.");
});

test("multiple_function_param_trailing_comma", () => {
    const text = `
		contract test {
			function(uint a, uint b,) {}
		}
	`;
    checkParseError(text, "Unexpected trailing comma in parameter list.");
});

test("multiple_return_param_trailing_comma", () => {
    const text = `
		contract test {
			function() returns (uint a, uint b,) {}
		}
	`;
    checkParseError(text, "Unexpected trailing comma in parameter list.");
});

test("multiple_modifier_arg_trailing_comma", () => {
    const text = `
		contract test {
			modifier modTest(uint a, uint b,) { _; }
			function(uint a) {}
		}
	`;
    checkParseError(text, "Unexpected trailing comma in parameter list.");
});

test("multiple_event_arg_trailing_comma", () => {
    const text = `
		contract test {
			event Test(uint a, uint b,);
			function(uint a) {}
		}
	`;
    checkParseError(text, "Unexpected trailing comma in parameter list.");
});

test("function_no_body", () => {
    const text = `
		contract test {
			function functionName(bytes32 input) returns (bytes32 out);
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("missing_parameter_name_in_named_args", () => {
    const text = `
		contract test {
			function a(uint a, uint b, uint c) returns (uint r) { r = a * 100 + b * 10 + c * 1; }
			function b() returns (uint r) { r = a({: 1, : 2, : 3}); }
		}
	`;
    checkParseError(text, "Expected identifier");
});

test("missing_argument_in_named_args", () => {
    const text = `
		contract test {
			function a(uint a, uint b, uint c) returns (uint r) { r = a * 100 + b * 10 + c * 1; }
			function b() returns (uint r) { r = a({a: , b: , c: }); }
		}
	`;
    checkParseError(text, "Expected primary expression");
});

test("trailing_comma_in_named_args", () => {
    const text = `
		contract test {
			function a(uint a, uint b, uint c) returns (uint r) { r = a * 100 + b * 10 + c * 1; }
			function b() returns (uint r) { r = a({a: 1, b: 2, c: 3, }); }
		}
	`;
    checkParseError(text, "Unexpected trailing comma");
});

test("two_exact_functions", () => {
    const text = `
		contract test {
			function fun(uint a) returns(uint r) { return a; }
			function fun(uint a) returns(uint r) { return a; }
		}
	`;
    // with support of overloaded functions, during parsing,
    // we can't determine whether they match exactly, however
    // it will throw DeclarationError in following stage.
    expect(successParse(text)).toBeTruthy();
});

test("overloaded_functions", () => {
    const text = `
		contract test {
			function fun(uint a) returns(uint r) { return a; }
			function fun(uint a, uint b) returns(uint r) { return a + b; }
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("function_natspec_documentation", () => {
    const text = `
		contract test {
			uint256 stateVar;
			/// This is a test function
			function functionName(bytes32 input) returns (bytes32 out) {}
		}
	`;
    expect(successParse(text)).toBeTruthy();
    const { node: contract } = parseText(text);
    const funs = contract.definedFunctions;
    const fun = funs[0];
    expect(fun).toBeTruthy();
    checkFunctionNatspec(fun, "This is a test function");
});

test("function_normal_comments", () => {
    const text = `
		contract test {
			uint256 stateVar;
			// We won't see this comment
			function functionName(bytes32 input) returns (bytes32 out) {}
		}
	`;
    expect(successParse(text)).toBeTruthy();
    const { node: contract } = parseText(text);
    const funs = contract.definedFunctions;
    const fun = funs[0];
    expect(fun).toBeTruthy();
    expect(fun.documentation()).toBeFalsy();
});

test("multiple_functions_natspec_documentation", () => {
    const text = `
		contract test {
			uint256 stateVar;
			/// This is test function 1
			function functionName1(bytes32 input) returns (bytes32 out) {}
			/// This is test function 2
			function functionName2(bytes32 input) returns (bytes32 out) {}
			// nothing to see here
			function functionName3(bytes32 input) returns (bytes32 out) {}
			/// This is test function 4
			function functionName4(bytes32 input) returns (bytes32 out) {}
		}
	`;
    expect(successParse(text)).toBeTruthy();
    const { node: contract } = parseText(text);
    const funs = contract.definedFunctions;

    let fun: FunctionDefinition;

    fun = funs[0];
    expect(fun).toBeTruthy();
    checkFunctionNatspec(fun, "This is test function 1");

    fun = funs[1];
    expect(fun).toBeTruthy();
    checkFunctionNatspec(fun, "This is test function 2");

    fun = funs[2];
    expect(fun).toBeTruthy();
    expect(fun.documentation()).toBeFalsy();

    fun = funs[3];
    expect(fun).toBeTruthy();
    checkFunctionNatspec(fun, "This is test function 4");
});

test("multiline_function_documentation", () => {
    const text = `
		contract test {
			uint256 stateVar;
			/// This is a test function
			/// and it has 2 lines
			function functionName1(bytes32 input) returns (bytes32 out) {}
		}
	`;
    expect(successParse(text)).toBeTruthy();
    const { node: contract } = parseText(text);
    const funs = contract.definedFunctions;
    const fun = funs[0];
    checkFunctionNatspec(fun, "This is a test function\n" +
        " and it has 2 lines");
});

test("natspec_comment_in_function_body", () => {
    const text = `
		contract test {
			/// fun1 description
			function fun1(uint256 a) {
				var b;
				/// I should not interfere with actual natspec comments
				uint256 c;
				mapping(address=>bytes32) d;
				bytes7 name = "Solidity";
			}
			/// This is a test function
			/// and it has 2 lines
			function fun(bytes32 input) returns (bytes32 out) {}
		}
	`;
    expect(successParse(text)).toBeTruthy();
    const { node: contract } = parseText(text);
    const funs = contract.definedFunctions;
    let fun: FunctionDefinition;

    fun = funs[0];
    expect(fun).toBeTruthy();
    checkFunctionNatspec(fun, "fun1 description");

    fun = funs[1];
    expect(fun).toBeTruthy();
    checkFunctionNatspec(fun, "This is a test function\n" +
        " and it has 2 lines");
});

test("natspec_docstring_between_keyword_and_signature", () => {
    const text = `
		contract test {
			uint256 stateVar;
			function ///I am in the wrong place
			fun1(uint256 a) {
				var b;
				/// I should not interfere with actual natspec comments
				uint256 c;
				mapping(address=>bytes32) d;
				bytes7 name = "Solidity";
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
    const { node: contract } = parseText(text);
    const funs = contract.definedFunctions;
    const fun = funs[0];
    expect(fun).toBeTruthy();
    expect(fun.documentation()).toBeFalsy();
});

test("natspec_docstring_after_signature", () => {
    const text = `
		contract test {
			uint256 stateVar;
			function fun1(uint256 a) {
				/// I should have been above the function signature
				var b;
				/// I should not interfere with actual natspec comments
				uint256 c;
				mapping(address=>bytes32) d;
				bytes7 name = "Solidity";
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
    const { node: contract } = parseText(text);
    const funs = contract.definedFunctions;
    const fun = funs[0];

    expect(fun).toBeTruthy();
    expect(fun.documentation()).toBeFalsy();
});

test("struct_definition", () => {
    const text = `
		contract test {
			uint256 stateVar;
			struct MyStructName {
				address addr;
				uint256 count;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("mapping", () => {
    const text = `
		contract test {
			mapping(address => bytes32) names;
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("mapping_in_struct", () => {
    const text = `
		contract test {
			struct test_struct {
				address addr;
				uint256 count;
				mapping(bytes32 => test_struct) self_reference;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("mapping_to_mapping_in_struct", () => {
    const text = `
		contract test {
			struct test_struct {
				address addr;
				mapping (uint64 => mapping (bytes32 => uint)) complex_mapping;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("variable_definition", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				var b;
				uint256 c;
				mapping(address=>bytes32) d;
				customtype varname;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("variable_definition_with_initialization", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				var b = 2;
				uint256 c = 0x87;
				mapping(address=>bytes32) d;
				bytes7 name = "Solidity";
				customtype varname;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("variable_definition_in_function_parameter", () => {
    const text = `
		contract test {
			function fun(var a) {}
		}
	`;
    checkParseError(text, "Expected explicit type name");
});

test("variable_definition_in_mapping", () => {
    const text = `
		contract test {
			function fun() {
				mapping(var=>bytes32) d;
			}
		}
	`;
    checkParseError(text, "Expected elementary type name for mapping key type");
});

test("variable_definition_in_function_return", () => {
    const text = `
		contract test {
			function fun() returns(var d) {
				return 1;
			}
		}
	`;
    checkParseError(text, "Expected explicit type name");
});

test("operator_expression", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				uint256 x = (1 + 4) || false && (1 - 12) + -9;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("complex_expression", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				uint256 x = (1 + 4).member(++67)[a/=9] || true;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("exp_expression", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				uint256 x = 3 ** a;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("while_loop", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				while (true) { uint256 x = 1; break; continue; } x = 9;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("for_loop_vardef_initexpr", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				for (uint256 i = 0; i < 10; i++) {
					uint256 x = i; break; continue;
				}
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("for_loop_simple_initexpr", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				uint256 i =0;
				for (i = 0; i < 10; i++) {
					uint256 x = i; break; continue;
				}
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("for_loop_simple_noexpr", () => {
    const text = `
		contract test {
				function fun(uint256 a) {
					uint256 i =0;
					for (;;) {
						uint256 x = i; break; continue;
					}
				}
			}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("for_loop_single_stmt_body", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				uint256 i = 0;
				for (i = 0; i < 10; i++)
					continue;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("if_statement", () => {
    const text = `
		contract test {
			function fun(uint256 a) {
				if (a >= 8) { return 2; } else { var b = 7; }
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("else_if_statement", () => {
    const text = `
		contract test {
			function fun(uint256 a) returns (address b) {
				if (a < 0) b = 0x67; else if (a == 0) b = 0x12; else b = 0x78;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("statement_starting_with_type_conversion", () => {
    const text = `
		contract test {
			function fun() {
				uint64(2);
				uint64[7](3);
				uint64[](3);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("type_conversion_to_dynamic_array", () => {
    const text = `
		contract test {
			function fun() {
				var x = uint64[](3);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("import_directive", () => {
    const text = `
		import "abc";
		contract test {
			function fun() {
				uint64(2);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("multiple_contracts", () => {
    const text = `
		contract test {
			function fun() {
				uint64(2);
			}
		}
		contract test2 {
			function fun() {
				uint64(2);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("multiple_contracts_and_imports", () => {
    const text = `
		import "abc";
		contract test {
			function fun() {
				uint64(2);
			}
		}
		import "def";
		contract test2 {
			function fun() {
				uint64(2);
			}
		}
		import "ghi";
	`;
    expect(successParse(text)).toBeTruthy();
});

test("contract_inheritance", () => {
    const text = `
		contract base {
			function fun() {
				uint64(2);
			}
		}
		contract derived is base {
			function fun() {
				uint64(2);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("contract_multiple_inheritance", () => {
    const text = `
		contract base {
			function fun() {
				uint64(2);
			}
		}
		contract derived is base, nonExisting {
			function fun() {
				uint64(2);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("contract_multiple_inheritance_with_arguments", () => {
    const text = `
		contract base {
			function fun() {
				uint64(2);
			}
		}
		contract derived is base(2), nonExisting("abc", "def", base.fun()) {
			function fun() {
				uint64(2);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("placeholder_in_function_context", () => {
    const text = `
		contract c {
			function fun() returns (uint r) {
				var _ = 8;
				return _ + 1;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("modifier", () => {
    const text = `
		contract c {
			modifier mod { if (msg.sender == 0) _; }
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("modifier_without_semicolon", () => {
    const text = `
		contract c {
			modifier mod { if (msg.sender == 0) _ }
		}
	`;
    checkParseError(text, "Expected token Semicolon got");
});

test("modifier_arguments", () => {
    const text = `
		contract c {
			modifier mod(address a) { if (msg.sender == a) _; }
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("modifier_invocation", () => {
    const text = `
		contract c {
			modifier mod1(uint a) { if (msg.sender == a) _; }
			modifier mod2 { if (msg.sender == 2) _; }
			function f() mod1(7) mod2 { }
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("fallback_function", () => {
    const text = `
		contract c {
			function() { }
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("event", () => {
    const text = `
		contract c {
			event e();
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("event_arguments", () => {
    const text = `
		contract c {
			event e(uint a, bytes32 s);
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("event_arguments_indexed", () => {
    const text = `
		contract c {
			event e(uint a, bytes32 indexed s, bool indexed b);
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("visibility_specifiers", () => {
    const text = `
		contract c {
			uint private a;
			uint internal b;
			uint public c;
			uint d;
			function f() {}
			function f_priv() private {}
			function f_public() public {}
			function f_internal() internal {}
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("multiple_visibility_specifiers", () => {
    let text = `
		contract c {
			uint private internal a;
		}`;
    checkParseError(text, "Visibility already specified as \"private\".");
    text = `
		contract c {
			function f() private external {}
		}`;
    checkParseError(text, "Visibility already specified as \"private\".");
});

test("multiple_statemutability_specifiers", () => {
    let text = `
		contract c {
			function f() payable payable {}
		}`;
    checkParseError(text, "State mutability already specified as \"payable\".");
    text = `
		contract c {
			function f() constant constant {}
		}`;
    checkParseError(text, "State mutability already specified as \"view\".");
    text = `
		contract c {
			function f() constant view {}
		}`;
    checkParseError(text, "State mutability already specified as \"view\".");
    text = `
		contract c {
			function f() payable constant {}
		}`;
    checkParseError(text, "State mutability already specified as \"payable\".");
    text = `
		contract c {
			function f() pure payable {}
		}`;
    checkParseError(text, "State mutability already specified as \"pure\".");
    text = `
		contract c {
			function f() pure constant {}
		}`;
    checkParseError(text, "State mutability already specified as \"pure\".");
});

test("literal_constants_with_ether_subdenominations", () => {
    const text = `
		contract c {
			function c ()
			{
				 a = 1 wei;
				 b = 2 szabo;
				 c = 3 finney;
				 b = 4 ether;
			}
			uint256 a;
			uint256 b;
			uint256 c;
			uint256 d;
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("literal_constants_with_ether_subdenominations_in_expressions", () => {
    const text = `
		contract c {
			function c ()
			{
				 a = 1 wei * 100 wei + 7 szabo - 3;
			}
			uint256 a;
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("enum_valid_declaration", () => {
    const text = `
		contract c {
			enum validEnum { Value1, Value2, Value3, Value4 }
			function c ()
			{
				a = foo.Value3;
			}
			uint256 a;
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("empty_enum_declaration", () => {
    const text = `
		contract c {
			enum foo { }
		}`;
    checkParseError(text, "enum with no members is not allowed");
});

test("malformed_enum_declaration", () => {
    const text = `
		contract c {
			enum foo { WARNING,}
		}`;
    checkParseError(text, "Expected Identifier after");
});

test("external_function", () => {
    const text = `
		contract c {
			function x() external {}
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("external_variable", () => {
    const text = `
		contract c {
			uint external x;
		}`;
    checkParseError(text, "Expected identifier");
});

test("arrays_in_storage", () => {
    const text = `
		contract c {
			uint[10] a;
			uint[] a2;
			struct x { uint[2**20] b; y[0] c; }
			struct y { uint d; mapping(uint=>x)[] e; }
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("arrays_in_events", () => {
    const text = `
		contract c {
			event e(uint[10] a, bytes7[8] indexed b, c[3] x);
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("arrays_in_expressions", () => {
    const text = `
		contract c {
			function f() { c[10] a = 7; uint8[10 * 2] x; }
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("multi_arrays", () => {
    const text = `
		contract c {
			mapping(uint => mapping(uint => int8)[8][][9])[] x;
		}`;
    expect(successParse(text)).toBeTruthy();
});

test("constant_is_keyword", () => {
    const text = `
		contract Foo {
			uint constant = 4;
	}`;
    checkParseError(text, "Expected identifier");
});

test("var_array", () => {
    const text = `
		contract Foo {
			function f() { var[] a; }
	}`;
    checkParseError(text, "Expected identifier");
});

test("location_specifiers_for_params", () => {
    const text = `
		contract Foo {
			function f(uint[] storage constant x, uint[] memory y) { }
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("location_specifiers_for_locals", () => {
    const text = `
		contract Foo {
			function f() {
				uint[] storage x;
				uint[] memory y;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("location_specifiers_for_state", () => {
    const text = `
		contract Foo {
			uint[] memory x;
	}`;
    checkParseError(text, "Expected identifier");
});

test("location_specifiers_with_var", () => {
    const text = `
		contract Foo {
			function f() { var memory x; }
	}`;
    checkParseError(text, "Location specifier needs explicit type name");
});

test("empty_comment", () => {
    const text = `
		//
		contract test
		{}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("comment_end_with_double_star", () => {
    const text = `
		contract C1 {
		/**
		 **/
		}
		contract C2 {}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("library_simple", () => {
    const text = `
		library Lib {
			function f() { }
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("local_const_variable", () => {
    const text = `
		contract Foo {
			function localConst() returns (uint ret)
			{
				uint constant local = 4;
				return local;
			}
	}`;
    checkParseError(text, "Expected token Semicolon");
});

test("multi_variable_declaration", () => {
    const text = `
		contract C {
			function f() {
				var (a,b,c) = g();
				var (d) = 2;
				var (,e) = 3;
				var (f,) = 4;
				var (x,,) = g();
				var (,y,) = g();
				var () = g();
				var (,,) = g();
			}
			function g() returns (uint, uint, uint) {}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("tuples", () => {
    const text = `
		contract C {
			function f() {
				uint a = (1);
				var (b,) = (1,);
				var (c,d) = (1, 2 + a);
				var (e,) = (1, 2, b);
				(a) = 3;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("tuples_without_commas", () => {
    const text = `
		contract C {
			function f() {
				var a = (2 2);
			}
		}
	`;
    checkParseError(text, "Expected token Comma");
});

test("member_access_parser_ambiguity", () => {
    const text = `
		contract C {
			struct S { uint a; uint b; uint[][][] c; }
			function f() {
				C.S x;
				C.S memory y;
				C.S[10] memory z;
				C.S[10](x);
				x.a = 2;
				x.c[1][2][3] = 9;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("using_for", () => {
    const text = `
		contract C {
			struct s { uint a; }
			using LibraryName for uint;
			using Library2 for *;
			using Lib for s;
			function f() {
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("complex_import", () => {
    const text = `
		import "abc" as x;
		import * as x from "abc";
		import {a as b, c as d, f} from "def";
		contract x {}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("from_is_not_keyword", () => {
    // "from" is not a keyword although it is used as a keyword in import directives.
    const text = `
		contract from {
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("inline_array_declaration", () => {
    const text = `
		contract c {
			uint[] a;
			function f() returns (uint, uint) {
				a = [1,2,3];
				return (a[3], [2,3,4][0]);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("inline_array_empty_cells_check_lvalue", () => {
    const text = `
		contract c {
			uint[] a;
			function f() returns (uint) {
				a = [,2,3];
				return (a[0]);
			}
		}
	`;
    checkParseError(text, "Expected expression");
});

test("inline_array_empty_cells_check_without_lvalue", () => {
    const text = `
		contract c {
			uint[] a;
			function f() returns (uint, uint) {
				return ([3, ,4][0]);
			}
		}
	`;
    checkParseError(text, "Expected expression");
});

test("conditional_true_false_literal", () => {
    const text = `
		contract A {
			function f() {
				uint x = true ? 1 : 0;
				uint y = false ? 0 : 1;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("conditional_with_constants", () => {
    const text = `
		contract A {
			function f() {
				uint x = 3 > 0 ? 3 : 0;
				uint y = (3 > 0) ? 3 : 0;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("conditional_with_variables", () => {
    const text = `
		contract A {
			function f() {
				uint x = 3;
				uint y = 1;
				uint z = (x > y) ? x : y;
				uint w = x > y ? x : y;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("conditional_multiple", () => {
    const text = `
		contract A {
			function f() {
				uint x = 3 < 0 ? 2 > 1 ? 2 : 1 : 7 > 2 ? 7 : 6;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("conditional_with_assignment", () => {
    const text = `
		contract A {
			function f() {
				uint y = 1;
				uint x = 3 < 0 ? x = 3 : 6;
				true ? x = 3 : 4;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("recursion_depth1", () => {
    let text = "contract C { bytes";
    for (let i = 0; i < 30000; i++)
        text += "[";
    checkParseError(text, "Maximum recursion depth reached during parsing");
});

test("recursion_depth2", () => {
    let text = "contract C { function f() {";
    for (let i = 0; i < 30000; i++)
        text += "{";
    checkParseError(text, "Maximum recursion depth reached during parsing");
});

test("recursion_depth3", () => {
    let text = "contract C { function f() { uint x = f(";
    for (let i = 0; i < 30000; i++)
        text += "(";
    checkParseError(text, "Maximum recursion depth reached during parsing");
});

test("recursion_depth4", () => {
    let text = "contract C { function f() { uint a;";
    for (let i = 0; i < 30000; i++)
        text += "(";
    text += "a";
    for (let i = 0; i < 30000; i++)
        text += "++)";
    text += "}}";
    checkParseError(text, "Maximum recursion depth reached during parsing");
});

test("declaring_fixed_and_ufixed_variables", () => {
    const text = `
		contract A {
			fixed40x40 storeMe;
			function f(ufixed x, fixed32x32 y) {
				ufixed8x8 a;
				fixed b;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("declaring_fixed_literal_variables", () => {
    const text = `
		contract A {
			fixed40x40 pi = 3.14;
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("no_double_radix_in_fixed_literal", () => {
    const text = `
		contract A {
			fixed40x40 pi = 3.14.15;
		}
	`;
    checkParseError(text, "Expected token Semicolon");
});

test("invalid_fixed_conversion_leading_zeroes_check", () => {
    const text = `
		contract test {
			function f() {
				fixed a = 1.0x2;
			}
		}
	`;
    checkParseError(text, "Expected primary expression");
});

test("payable_accessor", () => {
    const text = `
		contract test {
			uint payable x;
		}
	`;
    checkParseError(text, "Expected identifier");
});

test("function_type_in_expression", () => {
    const text = `
		contract test {
			function f(uint x, uint y) returns (uint a) {}
			function g() {
				function (uint, uint) internal returns (uint) f1 = f;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("function_type_as_storage_variable", () => {
    const text = `
		contract test {
			function (uint, uint) internal returns (uint) f1;
		}
    `;
    expect(successParse(text)).toBeTruthy();
});

test("function_type_as_storage_variable_with_modifiers", () => {
    const text = `
		contract test {
			function (uint, uint) modifier1() returns (uint) f1;
		}
	`;
    checkParseError(text, "Expected token LBrace");
});

test("function_type_as_storage_variable_with_assignment", () => {
    const text = `
		contract test {
			function f(uint x, uint y) returns (uint a) {}
			function (uint, uint) internal returns (uint) f1 = f;
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("function_type_in_struct", () => {
    const text = `
		contract test {
			struct S {
				function (uint x, uint y) internal returns (uint a) f;
				function (uint, uint) external returns (uint) g;
				uint d;
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("function_type_as_parameter", () => {
    const text = `
		contract test {
			function f(function(uint) external returns (uint) g) internal returns (uint a) {
				return g(1);
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("calling_function", () => {
    const text = `
		contract test {
			function f() {
				function() returns(function() returns(function() returns(function() returns(uint)))) x;
				uint y;
				y = x()()()();
			}
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("mapping_and_array_of_functions", () => {
    const text = `
		contract test {
			mapping (address => function() internal returns (uint)) a;
			mapping (address => function() external) b;
			mapping (address => function() external[]) c;
			function() external[] d;
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("function_type_state_variable", () => {
    const text = `
		contract test {
			function() x;
			function() y = x;
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("scientific_notation", () => {
    const text = `
		contract test {
			uint256 a = 2e10;
			uint256 b = 2E10;
			uint256 c = 200e-2;
			uint256 d = 2E10 wei;
			uint256 e = 2.5e10;
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("interface", () => {
    const text = `
		interface Interface {
			function f();
		}
	`;
    expect(successParse(text)).toBeTruthy();
});

test("newInvalidTypeName", () => {
    const text = `
		contract C {
			function f() {
				new var;
			}
		}
	`;
    checkParseError(text, "Expected explicit type name");
});
