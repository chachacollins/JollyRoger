const token = @import("token.zig");
const std = @import("std");
const util = @import("utils.zig");

pub const Lexer = struct {
    //zls
    input: []const u8,
    position: usize,
    readPosition: usize,
    ch: u8,
    pub fn init(input: []const u8) Lexer {
        const lexer = Lexer{
            .input = input,
            .position = 0,
            .readPosition = 0,
            .ch = undefined,
        };
        lexer.readChar();
        return lexer;
    }
    fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }
    pub fn nextToken(self: Lexer) token.Token {
        var tok: token.Token = undefined;
        switch (self.ch) {
            '=' => {
                tok = newToken(token.TokenType.ASSIGN, '=');
            },
            ';' => {
                tok = newToken(token.TokenType.SEMICOLON, ';');
            },
            '(' => {
                tok = newToken(token.TokenType.LBRACE, '(');
            },

            ')' => {
                tok = newToken(token.TokenType.RBRACE, ')');
            },

            ',' => {
                tok = newToken(token.TokenType.COMMA, ',');
            },

            '+' => {
                tok = newToken(token.TokenType.PLUS, '+');
            },

            '{' => {
                tok = newToken(token.TokenType.LPAREN, '{');
            },

            '}' => {
                tok = newToken(token.TokenType.LPAREN, '{');
            },
            0 => {
                tok.Literal = "";
                tok.Type = token.TokenType.EOF;
            },
        }
        self.readChar();
        return tok;
    }
};

fn newToken(tokenType: token.TokenType, ch: u8) token.Token {
    const chstr = try util.numToString(ch);
    const t = token.Token{ .Type = tokenType, .Literal = chstr };
    return t;
}
test "TestNextToken" {
    //pls zls
    const TestStruct = struct {
        expectedType: token.TokenType,
        expectedLiteral: []const u8,
    };
    const input = "=+(){},;";
    const tests = [_]TestStruct{
        //zls
        TestStruct{ .expectedType = token.TokenType.ASSIGN, .expectedLiteral = "=" },
        TestStruct{ .expectedType = token.TokenType.PLUS, .expectedLiteral = "+" },
        TestStruct{ .expectedType = token.TokenType.LPAREN, .expectedLiteral = "(" },
        TestStruct{ .expectedType = token.TokenType.RPAREN, .expectedLiteral = ")" },

        TestStruct{ .expectedType = token.TokenType.LBRACE, .expectedLiteral = "{" },
        TestStruct{ .expectedType = token.TokenType.RBRACE, .expectedLiteral = "}" },
        TestStruct{ .expectedType = token.TokenType.COMMA, .expectedLiteral = "," },
        TestStruct{ .expectedType = token.TokenType.SEMICOLON, .expectedLiteral = ";" },
        TestStruct{ .expectedType = token.TokenType.EOF, .expectedLiteral = "" },
    };
    const lexer = Lexer.init(input);
    for (tests, 0..) |tt, i| {
        const tok = lexer.nextToken();
        if (tok.Type != tt.expectedType) {
            std.log.debug("test{d} failed - token type wrong. expected ={s}, got = {s}", .{ i, tt.expectedType, tok.Type });
        }

        if (tok.Literal != tt.expectedLiteral) {
            std.log.debug("test{d} failed - token Literal wrong. expected ={s}, got = {s}", .{ i, tt.expectedLiteral, tok.Literal });
        }
    }
}
