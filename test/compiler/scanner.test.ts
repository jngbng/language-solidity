import { CharStream, Scanner } from "../../src/compiler/parsing/scanner";
import { Token } from "../../src/compiler/parsing/token";

test("test_empty", () => {
    const scanner = new Scanner(new CharStream(""));
    expect(scanner.currentToken).toBe(Token.TokenName.EOS);
});

test("smoke_test", () => {
    const scanner = new Scanner(new CharStream("function break;765  \t  \"string1\",'string2'\nidentifier1"));
    expect(scanner.currentToken).toBe(Token.TokenName.Function);
    expect(scanner.next()).toBe(Token.TokenName.Break);
    expect(scanner.next()).toBe(Token.TokenName.Semicolon);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLiteral).toBe("765");
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("string1");
    expect(scanner.next()).toBe(Token.TokenName.Comma);
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("string2");
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.currentLiteral).toBe("identifier1");
    expect(scanner.next()).toBe(Token.TokenName.EOS);
});

test("string_escapes", () => {
    const scanner = new Scanner(new CharStream("  { \"a\\x61\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("aa");
});

test("string_escapes_with_zero", () => {
    const scanner = new Scanner(new CharStream("  { \"a\\x61\\x00abc\""));

    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("aa\0abc");
});

test("string_escape_illegal", () => {
    const scanner = new Scanner(new CharStream(" bla \"\\x6rf\" (illegalescape)"));
    expect(scanner.currentToken).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
    expect(scanner.currentLiteral).toBe("");
    // TODO recovery from illegal tokens should be improved
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
});

test("hex_numbers", () => {
    const scanner = new Scanner(new CharStream("var x = 0x765432536763762734623472346;"));
    expect(scanner.currentToken).toBe(Token.TokenName.Var);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Assign);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLiteral).toBe("0x765432536763762734623472346");
    expect(scanner.next()).toBe(Token.TokenName.Semicolon);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
});

test("octal_numbers", () => {
    const scanner = new Scanner(new CharStream("07"));
    expect(scanner.currentToken).toBe(Token.TokenName.Illegal);
    scanner.resetSource(new CharStream("007"), "");
    expect(scanner.currentToken).toBe(Token.TokenName.Illegal);
    scanner.resetSource(new CharStream("-07"), "");
    expect(scanner.currentToken).toBe(Token.TokenName.Sub);
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
    scanner.resetSource(new CharStream("-.07"), "");
    expect(scanner.currentToken).toBe(Token.TokenName.Sub);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    scanner.resetSource(new CharStream("0"), "");
    expect(scanner.currentToken).toBe(Token.TokenName.Number);
    scanner.resetSource(new CharStream("0.1"), "");
    expect(scanner.currentToken).toBe(Token.TokenName.Number);
});

test("scientific_notation", () => {
    const scanner = new Scanner(new CharStream("var x = 2e10;"));
    expect(scanner.currentToken).toBe(Token.TokenName.Var);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Assign);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLiteral).toBe("2e10");
    expect(scanner.next()).toBe(Token.TokenName.Semicolon);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
});

test("negative_numbers", () => {
    const scanner = new Scanner(new CharStream("var x = -.2 + -0x78 + -7.3 + 8.9 + 2e-2;"));
    expect(scanner.currentToken).toBe(Token.TokenName.Var);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Assign);
    expect(scanner.next()).toBe(Token.TokenName.Sub);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLiteral).toBe(".2");
    expect(scanner.next()).toBe(Token.TokenName.Add);
    expect(scanner.next()).toBe(Token.TokenName.Sub);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLiteral).toBe("0x78");
    expect(scanner.next()).toBe(Token.TokenName.Add);
    expect(scanner.next()).toBe(Token.TokenName.Sub);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLiteral).toBe("7.3");
    expect(scanner.next()).toBe(Token.TokenName.Add);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLiteral).toBe("8.9");
    expect(scanner.next()).toBe(Token.TokenName.Add);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLiteral).toBe("2e-2");
    expect(scanner.next()).toBe(Token.TokenName.Semicolon);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
});

test("locations", () => {
    const scanner = new Scanner(new CharStream("function_identifier has ; -0x743/*comment*/\n ident //comment"));
    expect(scanner.currentToken).toBe(Token.TokenName.Identifier);
    expect(scanner.currentLocation.start).toBe(0);
    expect(scanner.currentLocation.end).toBe(19);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.currentLocation.start).toBe(20);
    expect(scanner.currentLocation.end).toBe(23);
    expect(scanner.next()).toBe(Token.TokenName.Semicolon);
    expect(scanner.currentLocation.start).toBe(24);
    expect(scanner.currentLocation.end).toBe(25);
    expect(scanner.next()).toBe(Token.TokenName.Sub);
    expect(scanner.next()).toBe(Token.TokenName.Number);
    expect(scanner.currentLocation.start).toBe(27);
    expect(scanner.currentLocation.end).toBe(32);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.currentLocation.start).toBe(45);
    expect(scanner.currentLocation.end).toBe(50);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
});

test("ambiguities", () => {
    // test scanning of some operators which need look-ahead
    const scanner = new Scanner(new CharStream("<=" + "<" + "+ +=a++ =>" + "<<"));
    expect(scanner.currentToken).toBe(Token.TokenName.LessThanOrEqual);
    expect(scanner.next()).toBe(Token.TokenName.LessThan);
    expect(scanner.next()).toBe(Token.TokenName.Add);
    expect(scanner.next()).toBe(Token.TokenName.AssignAdd);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Inc);
    expect(scanner.next()).toBe(Token.TokenName.Arrow);
    expect(scanner.next()).toBe(Token.TokenName.SHL);
});

test("documentation_comments_parsed_begin", () => {
    const scanner = new Scanner(new CharStream("/// Send $(value / 1000) chocolates to the user"));
    expect(scanner.currentToken).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("Send $(value / 1000) chocolates to the user");
});

test("multiline_documentation_comments_parsed_begin", () => {
    const scanner = new Scanner(new CharStream("/** Send $(value / 1000) chocolates to the user*/"));
    expect(scanner.currentToken).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("Send $(value / 1000) chocolates to the user");
});

test("documentation_comments_parsed", () => {
    const scanner = new Scanner(new CharStream("some other tokens /// Send $(value / 1000) chocolates to the user"));
    expect(scanner.currentToken).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("Send $(value / 1000) chocolates to the user");
});

test("multiline_documentation_comments_parsed", () => {
    const scanner = new Scanner(new CharStream("some other tokens /**\n" +
        "* Send $(value / 1000) chocolates to the user\n" +
        "*/"));
    expect(scanner.currentToken).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("Send $(value / 1000) chocolates to the user");
});

test("multiline_documentation_no_stars", () => {
    const scanner = new Scanner(new CharStream("some other tokens /**\n" +
        " Send $(value / 1000) chocolates to the user\n" +
        "*/"));
    expect(scanner.currentToken).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("Send $(value / 1000) chocolates to the user");
});

test("multiline_documentation_whitespace_hell", () => {
    const scanner = new Scanner(new CharStream("some other tokens /** \t \r \n" +
        "\t \r  * Send $(value / 1000) chocolates to the user\n" +
        "*/"));
    expect(scanner.currentToken).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("Send $(value / 1000) chocolates to the user");
});

test("comment_before_eos", () => {
    const scanner = new Scanner(new CharStream("//"));
    expect(scanner.currentToken).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("");
});

test("documentation_comment_before_eos", () => {
    const scanner = new Scanner(new CharStream("///"));
    expect(scanner.currentToken).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("");
});

test("empty_multiline_comment", () => {
    const scanner = new Scanner(new CharStream("/**/"));
    expect(scanner.currentToken).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("");
});

test("empty_multiline_documentation_comment_before_eos", () => {
    const scanner = new Scanner(new CharStream("/***/"));
    expect(scanner.currentToken).toBe(Token.TokenName.EOS);
    expect(scanner.currentCommentLiteral).toBe("");
});

test("comments_mixed_in_sequence", () => {
    const scanner = new Scanner(new CharStream("hello_world ///documentation comment \n" +
        "//simple comment \n" +
        "<<"));
    expect(scanner.currentToken).toBe(Token.TokenName.Identifier);
    expect(scanner.next()).toBe(Token.TokenName.SHL);
    expect(scanner.currentCommentLiteral).toBe("documentation comment ");
});

test("ether_subdenominations", () => {
    const scanner = new Scanner(new CharStream("wei szabo finney ether"));
    expect(scanner.currentToken).toBe(Token.TokenName.SubWei);
    expect(scanner.next()).toBe(Token.TokenName.SubSzabo);
    expect(scanner.next()).toBe(Token.TokenName.SubFinney);
    expect(scanner.next()).toBe(Token.TokenName.SubEther);
});

test("time_subdenominations", () => {
    const scanner = new Scanner(new CharStream("seconds minutes hours days weeks years"));
    expect(scanner.currentToken).toBe(Token.TokenName.SubSecond);
    expect(scanner.next()).toBe(Token.TokenName.SubMinute);
    expect(scanner.next()).toBe(Token.TokenName.SubHour);
    expect(scanner.next()).toBe(Token.TokenName.SubDay);
    expect(scanner.next()).toBe(Token.TokenName.SubWeek);
    expect(scanner.next()).toBe(Token.TokenName.SubYear);
});

test("empty_comment", () => {
    const scanner = new Scanner(new CharStream("//\ncontract{}"));
    expect(scanner.currentCommentLiteral).toBe("");
    expect(scanner.currentToken).toBe(Token.TokenName.Contract);
    expect(scanner.next()).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.RBrace);
});

test("valid_unicode_string_escape", () => {
    const scanner = new Scanner(new CharStream("{ \"\\u00DAnicode\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("\xC3\x9Anicode");
});

test("valid_unicode_string_escape_7f", () => {
    const scanner = new Scanner(new CharStream("{ \"\\u007Fnicode\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("\x7Fnicode");
});

test("valid_unicode_string_escape_7ff", () => {
    const scanner = new Scanner(new CharStream("{ \"\\u07FFnicode\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("\xDF\xBFnicode");
});

test("valid_unicode_string_escape_ffff", () => {
    const scanner = new Scanner(new CharStream("{ \"\\uFFFFnicode\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("\xEF\xBF\xBFnicode");
});

test("invalid_short_unicode_string_escape", () => {
    const scanner = new Scanner(new CharStream("{ \"\\uFFnicode\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
});

test("valid_hex_literal", () => {
    const scanner = new Scanner(new CharStream("{ hex\"00112233FF\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.StringLiteral);
    expect(scanner.currentLiteral).toBe("\x00\x11\x22\x33\xFF");
});

test("invalid_short_hex_literal", () => {
    const scanner = new Scanner(new CharStream("{ hex\"00112233F\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
});

test("invalid_hex_literal_with_space", () => {
    const scanner = new Scanner(new CharStream("{ hex\"00112233FF \""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
});

test("invalid_hex_literal_with_wrong_quotes", () => {
    const scanner = new Scanner(new CharStream("{ hex\"00112233FF'"));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
});

test("invalid_hex_literal_nonhex_string", () => {
    const scanner = new Scanner(new CharStream("{ hex\"hello\""));
    expect(scanner.currentToken).toBe(Token.TokenName.LBrace);
    expect(scanner.next()).toBe(Token.TokenName.Illegal);
});
