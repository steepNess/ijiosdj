package tokenizer;

import error.ErrorCode;
import error.TokenizeError;
import util.Pos;

import java.util.HashMap;
import java.util.regex.Pattern;

public class Tokenizer {
    private StringIter it;

    public Tokenizer(StringIter it) {
        this.it = it;
    }

    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError 如果解析有异常则抛出
     */
    public Token nextToken() throws TokenizeError {
        it.readAll();

        // 跳过之前的所有空白字符
        skipSpaceCharacters();

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }

        char peek = it.peekChar();
        if (Character.isDigit(peek)) {
            return lexUIntOrDouble();
        } else if (Character.isAlphabetic(peek) || peek == '_') {
            return lexIdentOrKeyword();
        } else if (peek == '"') {
            return lexString();
        } else if (peek == '\'') {
            return lexChar();
        } else {
            return lexOperatorOrCommentOrUnknown();
        }
    }

    private Token lexOperatorOrCommentOrUnknown() throws TokenizeError {
        Pos startPos;
        switch (it.nextChar()) {
            case '+':
                return new Token(TokenType.PLUS, '+', it.previousPos(), it.currentPos());
            case '-':
                startPos = it.previousPos();
                if (it.peekChar() == '>') {
                    it.nextChar();
                    return new Token(TokenType.ARROW, "->", startPos, it.currentPos());
                }
                return new Token(TokenType.MINUS, '-', startPos, it.currentPos());
            case '*':
                return new Token(TokenType.MUL, '*', it.previousPos(), it.currentPos());
            case '/':
                startPos = it.previousPos();
                if (it.peekChar() == '/') {
                    it.nextChar();
//                    StringBuilder tmpToken = new StringBuilder();
//                    tmpToken.append("//");
                    while (it.nextChar() != '\n');
//                        tmpToken.append(it.nextChar());
//                    tmpToken.append(it.nextChar());
//                    return new Token(TokenType.COMMENT, tmpToken.toString(), startPos, it.currentPos());
                    return nextToken();
                }
                return new Token(TokenType.DIV, '/', it.previousPos(), it.currentPos());
            case '=':
                startPos = it.previousPos();
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.EQ, "==", startPos, it.currentPos());
                }
                return new Token(TokenType.ASSIGN, '=', startPos, it.currentPos());
            case '!':
                startPos = it.previousPos();
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.NEQ, "!=", startPos, it.currentPos());
                } else throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
            case '<':
                startPos = it.previousPos();
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.LE, "<=", startPos, it.currentPos());
                }
                return new Token(TokenType.LT, '<', startPos, it.currentPos());
            case '>':
                startPos = it.previousPos();
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.GE, ">=", startPos, it.currentPos());
                }
                return new Token(TokenType.GT, '>', startPos, it.currentPos());
            case '(':
                return new Token(TokenType.L_PAREN, '(', it.previousPos(), it.currentPos());
            case ')':
                return new Token(TokenType.R_PAREN, ')', it.previousPos(), it.currentPos());
            case '{':
                return new Token(TokenType.L_BRACE, '{', it.previousPos(), it.currentPos());
            case '}':
                return new Token(TokenType.R_BRACE, '}', it.previousPos(), it.currentPos());
            case ',':
                return new Token(TokenType.COMMA, ',', it.previousPos(), it.currentPos());
            case ':':
                return new Token(TokenType.COLON, ':', it.previousPos(), it.currentPos());
            case ';':
                return new Token(TokenType.SEMICOLON, ';', it.previousPos(), it.currentPos());
            default:
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
    }

    private Token lexChar() throws TokenizeError {
        Pos startPos = it.currentPos();
        char value;
        it.nextChar();
        char peek = it.peekChar();
        if (peek != '\'') {
            if (peek == '\\')
            {
                it.nextChar();
                switch (it.nextChar()) {
                    case '\\':
                        value = '\\';
                        break;
                    case '"':
                        value = '"';
                        break;
                    case '\'':
                        value = '\'';
                        break;
                    case 'n':
                        value = '\n';
                        break;
                    case 'r':
                        value = '\r';
                        break;
                    case 't':
                        value = '\t';
                        break;
                    default:
                        throw new TokenizeError(ErrorCode.IllegalEscapeSequence, it.previousPos());
                }
            }
            else value = it.nextChar();
            if (it.peekChar() == '\'') {
                it.nextChar();
                return new Token(TokenType.CHAR_LITERAL, value, startPos, it.currentPos());
            } else throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
        throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
    }

    private Token lexString() throws TokenizeError {
        Pos startPos = it.currentPos();
        StringBuilder token = new StringBuilder();
        it.nextChar();
        char peek;
        while (!it.isEOF() && (peek = it.peekChar()) != '"')
        {
            if (peek == '\\') {
                it.nextChar();
                switch (it.nextChar()) {
                    case '\\':
                        token.append('\\');
                        break;
                    case '"':
                        token.append('"');
                        break;
                    case '\'':
                        token.append('\'');
                        break;
                    case 'n':
                        token.append('\n');
                        break;
                    case 'r':
                        token.append('\r');
                        break;
                    case 't':
                        token.append('\t');
                        break;
                    default:
                        throw new TokenizeError(ErrorCode.IllegalEscapeSequence, it.previousPos());
                }
            }
            else token.append(it.nextChar());
        }
        if (it.isEOF()) {
            throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
        it.nextChar();
        return new Token(TokenType.STRING_LITERAL, token.toString(), startPos, it.currentPos());
    }


    public final static HashMap<String,TokenType> keywordmap=new HashMap(){{
        put("fn", TokenType.FN_KW);
        put("let", TokenType.LET_KW);
        put("const", TokenType.CONST_KW);
        put("as", TokenType.AS_KW);
        put("while", TokenType.WHILE_KW);
        put("if", TokenType.IF_KW);
        put("else", TokenType.ELSE_KW);
        put("return", TokenType.RETURN_KW);
        put("break",TokenType.BREAK_KW);
        put("continue",TokenType.CONTINUE_KW);
        put("int",TokenType.INT_TY);
        put("void",TokenType.VOID_TY);
        put("double",TokenType.DOUBLE_TY);
    }};
    private Token lexIdentOrKeyword() throws TokenizeError {
        String token = "";
        token+=it.nextChar();
        Pos startPos=it.currentPos();
        while(Character.isLetterOrDigit(it.peekChar()) || it.peekChar() == '_'){
            token+=it.nextChar();
        }
        Pos endPos=it.currentPos();
        for(String key:keywordmap.keySet()){
            if(token.equals(key)){
                return new Token(keywordmap.get(key), token, startPos, endPos);
            }
        }
        return new Token(TokenType.IDENT, token, startPos, endPos);
    }

    private Token lexUIntOrDouble() throws TokenizeError {

        String token = "" ;
        while (Character.isDigit(it.peekChar()) || it.peekChar()=='.' || it.peekChar() == 'e' || it.peekChar() == 'E' || it.peekChar() == '+' || it.peekChar() == '-') {
            token += it.nextChar();
        }
        String doubleLiteral = "[0-9]+.[0-9]+([eE][-+]?[0-9]+)?";
        String uintLiteral = "[0-9]+";
        if(Pattern.matches(uintLiteral, token))
            return new Token(TokenType.UINT_LITERAL, Long.valueOf(token), it.previousPos(), it.currentPos());
        else if(Pattern.matches(doubleLiteral, token))
            return new Token(TokenType.DOUBLE_LITERAL, Double.valueOf(token), it.previousPos(), it.currentPos());
        else
            throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
    }

    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }
}
