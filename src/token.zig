pub const Token = struct {
    Type: TokenType,
    Literal: []const u8,
};

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    //Identifiers + Literals
    IDENT,
    INT,
    //operators
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    //
    FUNCTION,
    LET,
};
