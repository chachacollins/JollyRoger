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
        var lexer = Lexer{
            .input = input,
            .position = 0,
            .readPosition = 0,
            .ch = undefined,
        };
        lexer.readChar();
        return lexer;
    }
    pub fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }
    pub fn nextToken(self: *Lexer) !token.Token {
        var tok: token.Token = token.Token{
            .Literal = undefined,
            .Type = undefined,
        };
        switch (self.ch) {
            '=' => {
                tok.Type = token.TokenType.ASSIGN;
                tok.Literal = "=";
            },
            ';' => {
                tok.Type = token.TokenType.SEMICOLON;
                tok.Literal = ";";
            },
            '(' => {
                tok.Type = token.TokenType.LPAREN;
                tok.Literal = "(";
            },

            ')' => {
                tok.Type = token.TokenType.RPAREN;
                tok.Literal = ")";
            },

            ',' => {
                tok.Type = token.TokenType.COMMA;
                tok.Literal = ",";
            },

            '+' => {
                tok.Type = token.TokenType.PLUS;
                tok.Literal = "+";
            },

            '{' => {
                tok.Type = token.TokenType.LBRACE;
                tok.Literal = "{";
            },

            '}' => {
                tok.Type = token.TokenType.RBRACE;
                tok.Literal = "}";
            },
            0 => {
                tok.Literal = "";
                tok.Type = token.TokenType.EOF;
            },
            else => {},
        }
        self.readChar();
        return tok;
    }
};

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
    var lexer = Lexer.init(input);
    for (tests) |tt| {
        const tok = try lexer.nextToken();

        try std.testing.expectEqual(tt.expectedType, tok.Type);

        try std.testing.expectEqual(tt.expectedLiteral, tok.Literal);
    }
}
