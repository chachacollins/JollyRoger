const token = @import("token.zig");
const std = @import("std");

pub const Lexer = struct {
    //zls
    input: []const u8,
    position: usize,
    readPosition: usize,
    ch: u8,
    pub fn init(input: []const u8) Lexer {
        token.init() catch {};
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
    fn readDigit(self: *Lexer) []const u8 {
        const position = self.position;
        while (std.ascii.isDigit(self.ch)) {
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
    fn skipWhiteSpace(self: *Lexer) void {
        if (std.ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
    }

    pub fn nextToken(self: *Lexer) !token.Token {
        var tok: token.Token = token.Token{
            .Literal = undefined,
            .Type = undefined,
        };
        self.skipWhiteSpace();
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
                    tok.Type = token.lookUpIdent(tok.Literal);
                    return tok;
                } else if (std.ascii.isDigit(self.ch)) {
                    tok.Type = token.TokenType.INT;
                    tok.Literal = self.readDigit();
                    return tok;
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
        \\let six = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\x + y;
        \\};
        \\let result = add(five, ten);
    ;
    const tests = [_]TestStruct{
        //zls
        TestStruct{ .expectedType = token.TokenType.LET, .expectedLiteral = "let" },
        TestStruct{ .expectedType = token.TokenType.IDENT, .expectedLiteral = "six" },
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
        try std.testing.expectEqual(tt.expectedType, tok.Type);
        // try std.testing.expectEqual(tt.expectedLiteral, tok.Literal);
        if (!std.mem.eql(u8, tt.expectedLiteral, tok.Literal)) {
            std.debug.print("got tok: {s} but expectedLiteral {s}\n", .{ tok.Literal, tt.expectedLiteral });
            std.process.exit(1);
        }
        // std.debug.print("tok: {s}\n", .{tok.Literal});
        // std.debug.print("tok expectedLiteral: {s}\n", .{tt.expectedLiteral});
        //
        // std.debug.print("tok  type: {}\n", .{tok.Type});
        // std.debug.print("tok expectedType: {}\n", .{tt.expectedType});
        //
        // std.debug.print("--------------------------------------------------------\n", .{});
    }
}
