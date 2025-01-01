const std = @import("std");
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
    MINUS,
    BANG,
    ASTERIS,
    SLASH,
    LT,
    GT,
    //keywords
    FUNCTION,
    LET,
    RETURN,
    IF,
    ELSE,
    TRUE,
    FALSE,
    EQ,
    NOT_EQ,
};

pub var keywords = std.StringHashMap(TokenType).init(std.heap.page_allocator);

fn insertKeywords(key: []const u8, value: TokenType) !void {
    try keywords.put(key, value);
}
pub fn lookUpIdent(key: []const u8) TokenType {
    return keywords.get(key) orelse TokenType.IDENT;
}

pub fn init() !void {
    try insertKeywords("fn", TokenType.FUNCTION);
    try insertKeywords("let", TokenType.LET);
    try insertKeywords("if", TokenType.IF);
    try insertKeywords("else", TokenType.ELSE);
    try insertKeywords("return", TokenType.RETURN);
    try insertKeywords("true", TokenType.TRUE);
    try insertKeywords("false", TokenType.FALSE);
}
