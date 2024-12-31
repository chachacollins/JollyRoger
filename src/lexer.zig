const token = @import("token.zig");
const std = @import("std");

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
    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }
    fn isLetter(ch: u8) bool {
        if (std.ascii.isAlphabetic(ch) or ch == '_') {
            return true;
        } else {
            return false;
        }
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
            else => {
                if (isLetter(self.ch)) {
                    tok.Literal = self.readIdentifier();
                }
            },
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
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\x + y;
        \\};
        \\let result = add(five, ten);
    ;
    const tests = [_]TestStruct{
        //zls
        TestStruct{ .expectedType = token.TokenType.LET, .expectedLiteral = "let" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "five" },
        TestStruct{ .expectedType = token.TokenType.ASSIGN, .expectedLiteral = "=" },
        TestStruct{ .expectedType = token.TokenType.INT, .expectedLiteral = "5" },
        TestStruct{ .expectedType = token.TokenType.SEMICOLON, .expectedLiteral = ";" },
        TestStruct{ .expectedType = token.TokenType.LET, .expectedLiteral = "let" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "ten" },
        TestStruct{ .expectedType = token.TokenType.ASSIGN, .expectedLiteral = "=" },
        TestStruct{ .expectedType = token.TokenType.INT, .expectedLiteral = "10" },
        TestStruct{ .expectedType = token.TokenType.SEMICOLON, .expectedLiteral = ";" },
        TestStruct{ .expectedType = token.TokenType.LET, .expectedLiteral = "let" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "add" },
        TestStruct{ .expectedType = token.TokenType.ASSIGN, .expectedLiteral = "=" },
        TestStruct{ .expectedType = token.TokenType.FUNCTION, .expectedLiteral = "fn" },
        TestStruct{ .expectedType = token.TokenType.LPAREN, .expectedLiteral = "(" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "x" },
        TestStruct{ .expectedType = token.TokenType.COMMA, .expectedLiteral = "," },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "y" },
        TestStruct{ .expectedType = token.TokenType.RPAREN, .expectedLiteral = ")" },
        TestStruct{ .expectedType = token.TokenType.LBRACE, .expectedLiteral = "{" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "x" },
        TestStruct{ .expectedType = token.TokenType.PLUS, .expectedLiteral = "+" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "y" },
        TestStruct{ .expectedType = token.TokenType.SEMICOLON, .expectedLiteral = ";" },
        TestStruct{ .expectedType = token.TokenType.RBRACE, .expectedLiteral = "}" },
        TestStruct{ .expectedType = token.TokenType.SEMICOLON, .expectedLiteral = ";" },
        TestStruct{ .expectedType = token.TokenType.LET, .expectedLiteral = "let" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "result" },
        TestStruct{ .expectedType = token.TokenType.ASSIGN, .expectedLiteral = "=" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "add" },
        TestStruct{ .expectedType = token.TokenType.LPAREN, .expectedLiteral = "(" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "five" },
        TestStruct{ .expectedType = token.TokenType.COMMA, .expectedLiteral = "," },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "ten" },
        TestStruct{ .expectedType = token.TokenType.RPAREN, .expectedLiteral = ")" },
        TestStruct{ .expectedType = token.TokenType.SEMICOLON, .expectedLiteral = ";" },
        TestStruct{ .expectedType = token.TokenType.EOF, .expectedLiteral = "" },
    };
    var lexer = Lexer.init(input);
    for (tests) |tt| {
        const tok = try lexer.nextToken();
        // try std.testing.expectEqual(tt.expectedType, tok.Type);
        // try std.testing.expectEqual(tt.expectedLiteral, tok.Literal);
        std.debug.print("tok {s}\n", .{tok.Literal});
        std.debug.print("tok expectedLiteral {s}\n", .{tt.expectedLiteral});
    }
}
