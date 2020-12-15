package analyser;

import error.*;
import instruction.FunctionInstruction;
import instruction.Instruction;
import instruction.Operation;
import symbol.Scope;
import symbol.Symbol;
import symbol.Type;
import tokenizer.Token;
import tokenizer.TokenType;
import tokenizer.Tokenizer;
import util.Pos;
import util.WriteFile;

import java.io.IOException;
import java.util.*;

public class Analyser {
    //词法分析器
    Tokenizer tokenizer;
    Token peekedToken = null;

    //OPG矩阵

    ArrayList<TokenType> terminals = new ArrayList<>(Arrays.asList(TokenType.GT,
            TokenType.LT, TokenType.GE, TokenType.LE, TokenType.EQ, TokenType.NEQ,
            TokenType.PLUS, TokenType.MINUS, TokenType.MUL, TokenType.DIV, TokenType.AS_KW));
    //1=less,2=more
    /*
    public int[][] matrix = {
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}};*/

    //散列符号表的栈式实现
    Stack<Symbol> symbolTable = new Stack<>();
    //分程序索引
    Stack<Integer> subProgramIndex = new Stack<>();
    //散列表 key:标识符 value:标识符在符号表中的索引
    HashMap<String, Integer> hashTable = new HashMap<>();

    //指令集
    ArrayList<Instruction> instructions;

    //start函数指令集
    ArrayList<Instruction> startInstructions;
    //全局变量表
    ArrayList<GlobalDef> globals;
    //全局常量+变量id
    int globalNo = 0;
    //参数变量id
    int argNo = 0;
    //局部变量id
    int localNo = 0;
    //函数id
    int funcNo = 1;

    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
        this.instructions = new ArrayList<>();
        this.startInstructions = new ArrayList<>();
        this.globals = new ArrayList<GlobalDef>();
        this.subProgramIndex.push(0);
    }

    public void analyse(String fileName) throws CompileError, IOException {
        analyseProgram();
        //debug
        for (GlobalDef global : globals) {
            System.out.println(global.array_items);
        }
        System.out.println();
        for (Instruction instruction : instructions) {
            System.out.println(instruction.toString());
        }
        System.out.println();
        for (Instruction startInstruction : startInstructions) {
            System.out.println(startInstruction.toString());
        }
        WriteFile.outFile(fileName, globals, instructions, startInstructions);
    }

    /**
     * 查看下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            Token token = peekedToken;
            peekedToken = null;
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则返回 true
     *
     * @param tt
     * @return
     * @throws TokenizeError
     */
    private boolean check(TokenType tt) throws TokenizeError {
        Token token = peek();
        return token.getTokenType() == tt;
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回这个 token
     *
     * @param tt 类型
     * @return 如果匹配则返回这个 token，否则返回 null
     * @throws TokenizeError
     */
    private Token nextIf(TokenType tt) throws TokenizeError {
        Token token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            return null;
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
     *
     * @param tt 类型
     * @return 这个 token
     * @throws CompileError 如果类型不匹配
     */
    private Token expect(TokenType tt) throws CompileError {
        Token token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    /**
     * 添加一个符号
     *
     * @param name          名字
     * @param isInitialized 是否已赋值
     * @param isConstant    是否是常量
     * @param curPos        当前 token 的位置（报错用）
     * @throws AnalyzeError 如果重复定义了则抛异常
     */
    private Symbol addSymbol(String name, boolean isConstant, boolean isInitialized, Type type, Scope scope, Pos curPos) throws AnalyzeError {
        //获取标识符在符号表中的存储单元地址
        Integer symTbIndex = this.hashTable.get(name);
        //同一分程序中的标识符存在非法的重复声明
        if (symTbIndex != null && symTbIndex >= this.subProgramIndex.peek())
        {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        }
        else if(type.equals(Type.FUNCTION))
        {
            Symbol symbol = new Symbol(name, true, Scope.global, globalNo++, funcNo++);
            this.symbolTable.push(symbol);
            this.hashTable.put(name, symbolTable.size() - 1);
            this.subProgramIndex.push(symbolTable.size());
            globals.add(new GlobalDef(1,name));
            System.out.println("add:" + symbolTable.peek().getName());
            return symbol;
        }
        else
        {
            switch (scope)
            {
                case global:
                    this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, symTbIndex, scope, globalNo++));
                    if (isConstant)
                        globals.add(new GlobalDef(1));
                    else
                        globals.add(new GlobalDef(0));
                    break;
                case argument:
                    this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, symTbIndex, scope, argNo++));
                    break;
                case local:
                    this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, symTbIndex, scope, localNo++));
                    break;
            }
            System.out.println("add/dup:" + symbolTable.peek().getName());
            //标识符重名，冲突记录用链域相连（冲突记录在符号表中的位置）
            /*
            if (symTbIndex != null)
            {
                switch (scope)
                {
                    case global:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, symTbIndex, scope, globalNo++));
                        if (isConstant)
                            globals.add(new GlobalDef(1));
                        else
                            globals.add(new GlobalDef(0));
                        break;
                    case argument:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, symTbIndex, scope, argNo++));
                        break;
                    case local:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, symTbIndex, scope, localNo++));
                        break;
                }
                System.out.println("add/dup:" + symbolTable.peek().getName());
            }

            else
            {
                switch (scope) {
                    case global:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, scope, globalNo++));
                        if (isConstant)
                            globals.add(new GlobalDef(1));
                        else
                            globals.add(new GlobalDef(0));
                        break;
                    case argument:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, scope, argNo++));
                        break;
                    case local:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, type, scope, localNo++));
                        break;
                }
                System.out.println("add:" + symbolTable.peek().getName());
            }*/
            //散列表存放标识符在符号表中的位置，冲突则以最新记录为准
            this.hashTable.put(name, symbolTable.size() - 1);
        }
        return this.symbolTable.peek();
    }


    //符号表定位 分程序索引项指向每个分程序的第一个记录项
    private void addBlock() {
        this.subProgramIndex.push(this.symbolTable.size());
        System.out.println("add block");
    }

    //符号表重定位
    private void relocate(boolean isFunction)
    {
        int start = subProgramIndex.pop();
        for (int i = symbolTable.size() - 1; i >= start; i--)
        {
            //从符号表中移除
            Symbol symbol = symbolTable.pop();
            //标识符无冲突 散列表中移除
            if (symbol.getConflictChain() == -1)
            {
                hashTable.remove(symbol.getName());
            }
            //标识符有冲突，散列表中恢复为冲突记录在符号表的位置
            else
            {
                hashTable.put(symbol.getName(), symbol.getConflictChain());
            }
        }
        if (isFunction)
            argNo = 0;
        System.out.println("remove block");
    }

    private void changeInitialized(String name, Pos curPos) throws AnalyzeError {
        Symbol symbol = symbolTable.get(hashTable.get(name));
        if (symbol.isConstant())
            throw new AnalyzeError(ErrorCode.AssignToConstant, curPos);
        else
        {
            if (!symbol.isInitialized())
                symbol.setInitialized(true);
        }
    }

    private void analyseProgram() throws CompileError {
        globals.add(new GlobalDef(1,"_start"));
        startInstructions.add(new FunctionInstruction(Operation.func, 0, 0, 0, globalNo++));
        // program -> decl_stmt* function*
        while (check(TokenType.FN_KW) || check(TokenType.LET_KW) || check(TokenType.CONST_KW)) {
            if (check(TokenType.FN_KW))
                analyseFunction();
            else
                analyseDeclStmt(Scope.global);
        }
        Token eof = expect(TokenType.EOF);
        if (this.hashTable.get("main") == null)
            throw new AnalyzeError(ErrorCode.NoMainSymbol, eof.getStartPos());
        startInstructions.add(new Instruction(Operation.stackalloc, 0));
        startInstructions.add(new Instruction(Operation.call, this.symbolTable.get(this.hashTable.get("main")).getFuncOffset()));
    }

    /**
     * 函数声明分析函数
     * function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
     * @throws CompileError
     */
    private void analyseFunction() throws CompileError {
        expect(TokenType.FN_KW);
        localNo = 0;
        Token IDENT = expect(TokenType.IDENT);
        //加入符号表，全局变量表，散列表，分程序索引表，globalNo++,funcNo++
        Symbol funcSymbol=addSymbol(IDENT.getValueString(),false,false,Type.FUNCTION,Scope.global,IDENT.getStartPos());
        FunctionInstruction functionInstruction = new FunctionInstruction(Operation.func);
        instructions.add(functionInstruction);
        expect(TokenType.L_PAREN);
        if (check(TokenType.IDENT))
            analyseFunctionParamList(funcSymbol.getParams());
        expect(TokenType.R_PAREN);
        expect(TokenType.ARROW);
        Type type = analyseType();
        funcSymbol.setSymbolType(type);
        functionInstruction.setParamSlots(argNo);
        if (type == Type.VOID)
            functionInstruction.setRetSlots(0);
        else {
            functionInstruction.setRetSlots(1);
            int last = symbolTable.size() - 1;
            for (int i = 0; i < argNo; i++) {
                Symbol symbol = this.symbolTable.get(last - i);
                symbol.setOffset(symbol.getOffset() + 1);
            }
        }
        functionInstruction.setOffset(funcSymbol.getOffset());
        boolean[] b = analyseBlockStmt(true, false, type, 0, null);
        if (type != Type.VOID && !b[0]) {
            throw new AnalyzeError(ErrorCode.MissingReturnStatement, IDENT.getStartPos());
        }
        if (type == Type.VOID && !b[0])
            instructions.add(new Instruction(Operation.ret));
        functionInstruction.setLocalSlots(localNo);
    }

    private void analyseFunctionParamList(ArrayList<Type> params) throws CompileError {
        do {
            analyseFunctionParam(params);
        } while (nextIf(TokenType.COMMA) != null);
    }

    //need implement
    private void analyseFunctionParam(ArrayList<Type> params) throws CompileError {
        boolean isConstant = false;
        if (nextIf(TokenType.CONST_KW) != null)
            isConstant = true;
        Token nameToken = expect(TokenType.IDENT);
        expect(TokenType.COLON);
        Type type = analyseType();
        addSymbol(nameToken.getValueString(), isConstant, true, type, Scope.argument, nameToken.getStartPos());
        params.add(type);
    }

    private boolean[] analyseBlockStmt(boolean isFunction, boolean insideWhile, Type returnType, int loopLoc, ArrayList<Integer> breakList) throws CompileError {
        boolean haveReturn = false;
        boolean haveBreakOrContinue = false;
        int returnSize = 0;
        int breakOrContinueSize = 0;
        expect(TokenType.L_BRACE);
        if (!isFunction)
            addBlock();
        while (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL) || check(TokenType.L_PAREN) || check(TokenType.LET_KW) ||
                check(TokenType.CONST_KW) || check(TokenType.IF_KW) || check(TokenType.WHILE_KW) || check(TokenType.BREAK_KW) || check(TokenType.CONTINUE_KW) || check(TokenType.RETURN_KW) || check(TokenType.SEMICOLON) || check(TokenType.L_BRACE)) {
            if (returnSize == 0 && haveReturn)
//                throw new AnalyzeError(ErrorCode.unreachableStatement, peek().getStartPos());
                returnSize = instructions.size();
            else if (breakOrContinueSize == 0 && haveBreakOrContinue)
                breakOrContinueSize = instructions.size();

            if (haveReturn && haveBreakOrContinue)
                analyseStmt(insideWhile, returnType, loopLoc, breakList);
            else if (haveReturn)
                haveBreakOrContinue = analyseStmt(insideWhile, returnType, loopLoc, breakList)[1];
            else if (haveBreakOrContinue)
                haveReturn = analyseStmt(insideWhile, returnType, loopLoc, breakList)[0];
            else {
                boolean[] b = analyseStmt(insideWhile, returnType, loopLoc, breakList);
                haveReturn = b[0];
                haveBreakOrContinue = b[1];
            }
        }
        expect(TokenType.R_BRACE);
        if (returnSize > 0)
            instructions.subList(returnSize, instructions.size()).clear();
        if (breakOrContinueSize > 0)
            instructions.subList(breakOrContinueSize, instructions.size()).clear();
//        if (isFunction && !haveReturn)
//            throw new AnalyzeError(ErrorCode.MissingReturnStatement, RBrace.getStartPos());
        relocate(isFunction);
        return new boolean[]{haveReturn, haveBreakOrContinue};
    }

    private boolean[] analyseStmt(boolean insideWhile, Type returnType, int loopLoc, ArrayList<Integer> breakList) throws CompileError {
        if (check(TokenType.CONST_KW) || check(TokenType.LET_KW))
            analyseDeclStmt(Scope.local);
        else if (check(TokenType.IF_KW))
            return analyseIfStmt(insideWhile, returnType, loopLoc, breakList);
        else if (check(TokenType.WHILE_KW))
            analyseWhileStmt(returnType);
        else if (check(TokenType.BREAK_KW)) {
            if (insideWhile)
                analyseBreakStmt(breakList);
            else
                throw new AnalyzeError(ErrorCode.BreakOutsideLoop, peek().getStartPos());
            return new boolean[]{false, true};
        } else if (check(TokenType.CONTINUE_KW)) {
            if (insideWhile)
                analyseContinueStmt(loopLoc);
            else
                throw new AnalyzeError(ErrorCode.ContinueOutsideLoop, peek().getStartPos());
            return new boolean[]{false, true};
        } else if (check(TokenType.RETURN_KW)) {
            analyseReturnStmt(returnType);
            return new boolean[]{true, false};
        } else if (check(TokenType.L_BRACE))
            return analyseBlockStmt(false, insideWhile, returnType, loopLoc, breakList);
        else if (check(TokenType.SEMICOLON))
            expect(TokenType.SEMICOLON);
        else
            analyseExprStmt();
        return new boolean[]{false, false};
    }

    private void analyseDeclStmt(Scope scope) throws CompileError {
        if (check(TokenType.CONST_KW))
            analyseConstDeclStmt(scope);
        else
            analyseLetDeclStmt(scope);
    }

    private void analyseConstDeclStmt(Scope scope) throws CompileError {
        boolean isGlobal = scope == Scope.global;
        expect(TokenType.CONST_KW);
        Token nameToken = expect(TokenType.IDENT);
        expect(TokenType.COLON);
        Type type = analyseType();
        if (type == Type.VOID)
            throw new AnalyzeError(ErrorCode.InvalidDeclaration, nameToken.getStartPos());
        Symbol symbol = addSymbol(nameToken.getValueString(), true, true, type, scope, nameToken.getStartPos());
        expect(TokenType.ASSIGN);

        if (isGlobal)
            startInstructions.add(new Instruction(Operation.globa, symbol.getOffset()));
        else
            instructions.add(new Instruction(Operation.loca, symbol.getOffset()));

        OPGElement element = analyseOPGExpr(isGlobal);
        if (type != element.getType())
            throw new AnalyzeError(ErrorCode.InvalidType, element.getStartPos());
        expect(TokenType.SEMICOLON);
        if (isGlobal)
            startInstructions.add(new Instruction(Operation.store64));
        else
            instructions.add(new Instruction(Operation.store64));
    }

    private void analyseLetDeclStmt(Scope scope) throws CompileError {
        boolean isGlobal = scope == Scope.global;
        expect(TokenType.LET_KW);
        Token nameToken = expect(TokenType.IDENT);
        expect(TokenType.COLON);
        Type type = analyseType();
        if (type == Type.VOID)
            throw new AnalyzeError(ErrorCode.InvalidDeclaration, nameToken.getStartPos());
        Symbol symbol = addSymbol(nameToken.getValueString(), false, false, type, scope, nameToken.getStartPos());
        if (nextIf(TokenType.ASSIGN) != null) {
            if (isGlobal)
                startInstructions.add(new Instruction(Operation.globa, symbol.getOffset()));
            else
                instructions.add(new Instruction(Operation.loca, symbol.getOffset()));

            OPGElement element = analyseOPGExpr(isGlobal);
            if (type != element.getType())
                throw new AnalyzeError(ErrorCode.InvalidType, element.getStartPos());
            changeInitialized(nameToken.getValueString(), nameToken.getStartPos());
            if (isGlobal)
                startInstructions.add(new Instruction(Operation.store64));
            else
                instructions.add(new Instruction(Operation.store64));
        }
        expect(TokenType.SEMICOLON);
    }

    private boolean[] analyseIfStmt(boolean insideWhile, Type returnType, int loopLoc, ArrayList<Integer> breakList) throws CompileError {
        boolean haveReturn;
        boolean haveBreakOrContinue;
        boolean haveElse = false;
        ArrayList<Integer> brToEnds = new ArrayList<>();
        expect(TokenType.IF_KW);
        OPGElement element = analyseOPGExpr(false);
        if (element.getType() == Type.VOID)
            throw new AnalyzeError(ErrorCode.InvalidType, element.getStartPos());
        instructions.add(new Instruction(Operation.brtrue, 1));
        instructions.add(new Instruction(Operation.br));
        int brLoc = instructions.size() - 1;
        boolean[] b = analyseBlockStmt(false, insideWhile, returnType, loopLoc, breakList);
        haveReturn = b[0];
        haveBreakOrContinue = b[1];
        brToEnds.add(instructions.size());
        instructions.add(new Instruction(Operation.br));
        instructions.get(brLoc).setX(instructions.size() - brLoc - 1);
        if (check(TokenType.ELSE_KW)) {
            while (nextIf(TokenType.ELSE_KW) != null) {
                if (nextIf(TokenType.IF_KW) != null) {
                    element = analyseOPGExpr(false);
                    if (element.getType() == Type.VOID)
                        throw new AnalyzeError(ErrorCode.InvalidType, element.getStartPos());
                    instructions.add(new Instruction(Operation.brtrue, 1));
                    instructions.add(new Instruction(Operation.br));
                    brLoc = instructions.size() - 1;
                    b = analyseBlockStmt(false, insideWhile, returnType, loopLoc, breakList);
                    haveReturn &= b[0];
                    haveBreakOrContinue &= b[1];
                    brToEnds.add(instructions.size());
                    instructions.add(new Instruction(Operation.br));
                    instructions.get(brLoc).setX(instructions.size() - brLoc - 1);
                } else {
                    b = analyseBlockStmt(false, insideWhile, returnType, loopLoc, breakList);
                    haveReturn &= b[0];
                    haveBreakOrContinue &= b[1];
                    haveElse = true;
                    break;
                }
            }
        }
        if (!haveElse) {
            haveReturn = false;
            haveBreakOrContinue = false;
        }
        for (Integer brToEnd : brToEnds) {
            instructions.get(brToEnd).setX(instructions.size() - brToEnd - 1);
        }
        return new boolean[]{haveReturn, haveBreakOrContinue};
    }

    private void analyseWhileStmt(Type returnType) throws CompileError {
        expect(TokenType.WHILE_KW);
        ArrayList<Integer> breakList = new ArrayList<>();
        int loopLoc = instructions.size() - 1;
        analyseOPGExpr(false);
        instructions.add(new Instruction(Operation.brtrue, 1));
        int brLoc = instructions.size();
        instructions.add(new Instruction(Operation.br));
        boolean haveBreakOrContinue = analyseBlockStmt(false, true, returnType, loopLoc, breakList)[1];
        if (!haveBreakOrContinue)
            instructions.add(new Instruction(Operation.br, loopLoc - instructions.size()));
        instructions.get(brLoc).setX(instructions.size() - brLoc - 1);
        for (Integer breakNum : breakList) {
            instructions.get(breakNum).setX(instructions.size() - breakNum - 1);
        }
    }

    private void analyseBreakStmt(ArrayList<Integer> breakList) throws CompileError {
        expect(TokenType.BREAK_KW);
        expect(TokenType.SEMICOLON);
        breakList.add(instructions.size());
        instructions.add(new Instruction(Operation.br));
    }

    private void analyseContinueStmt(int loopLoc) throws CompileError {
        expect(TokenType.CONTINUE_KW);
        expect(TokenType.SEMICOLON);
        instructions.add(new Instruction(Operation.br, loopLoc - instructions.size()));
    }

    private void analyseReturnStmt(Type returnType) throws CompileError {
        Token expect = expect(TokenType.RETURN_KW);
        if (returnType != Type.VOID)
            instructions.add(new Instruction(Operation.arga, 0));
        Type type = Type.VOID;
        if (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.DOUBLE_LITERAL) ||
                check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL) || check(TokenType.L_PAREN)) {
            OPGElement element = analyseOPGExpr(false);
            type = element.getType();
        }
        expect(TokenType.SEMICOLON);
        if (type != returnType)
            throw new AnalyzeError(ErrorCode.InvalidType, expect.getStartPos());
        if (type != Type.VOID)
            instructions.add(new Instruction(Operation.store64));
        instructions.add(new Instruction(Operation.ret));
    }

    private void analyseExprStmt() throws CompileError {
        OPGElement element = analyseOPGExpr(false);
        expect(TokenType.SEMICOLON);
        if (element.getType() != Type.VOID)
            instructions.add(new Instruction(Operation.pop));
    }

    //expr->
    private OPGElement analyseOPGExpr(boolean isGlobal) throws CompileError {
        Stack<TokenType> symbolStack = new Stack<>();
        Stack<OPGElement> exprStack = new Stack<>();
        //因为stack是TokenType类型的，因此用EOF代替OPG的#
        if (symbolStack.empty()) {
            symbolStack.push(TokenType.EOF);
            exprStack.push(analyseOtherExpr(isGlobal));
        }
        while (!symbolStack.empty()) {
            TokenType nextType = peek().getTokenType();
            int x = terminals.indexOf(symbolStack.peek());
            int y = terminals.indexOf(nextType);
            if (x == -1 && y == -1) break;
            else if (x == -1 || y != -1 && OpgMatrix.matrix[x][y] == 1) {
                symbolStack.push(nextType);
                next();
                if (nextType == TokenType.AS_KW) {
                    Type type = analyseType();
                    exprStack.push(new OPGElement(type, null));
                } else
                    exprStack.push(analyseOtherExpr(isGlobal));

            } else if (y == -1 ||OpgMatrix.matrix[x][y] == 2) {
                reduction(symbolStack, exprStack, isGlobal);
            }
//            }
        }
        return exprStack.peek();
    }

    private void reduction(Stack<TokenType> symbols, Stack<OPGElement> Exprs, boolean isGlobal) throws CompileError {
        if (Exprs.size() > 1) {
            Type secondType = Exprs.pop().getType();
            OPGElement first = Exprs.peek();
            TokenType type = symbols.pop();
            Type firstType = first.getType();
            if (TokenType.AS_KW == type) {
                if (firstType == Type.VOID || secondType == Type.VOID)
                    throw new AnalyzeError(ErrorCode.InvalidType, first.getStartPos());
                else {
                    if (firstType == Type.INT && secondType == Type.DOUBLE) {
                        first.setType(secondType);
                        if (isGlobal)
                            startInstructions.add(new Instruction(Operation.itof));
                        else
                            instructions.add(new Instruction(Operation.itof));
                    } else if (firstType == Type.DOUBLE && secondType == Type.INT) {
                        first.setType(secondType);
                        if (isGlobal)
                            startInstructions.add(new Instruction(Operation.ftoi));
                        else
                            instructions.add(new Instruction(Operation.ftoi));
                    }
                }
            } else {
                if (firstType == secondType) {
                    switch (type) {
                        case GT:
                            if (firstType == Type.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.setgt));
                            first.setType(Type.BOOL);
                            break;
                        case LT:
                            if (firstType == Type.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.setlt));
                            first.setType(Type.BOOL);
                            break;
                        case GE:
                            if (firstType == Type.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.setlt));
                            instructions.add(new Instruction(Operation.not));
                            first.setType(Type.BOOL);
                            break;
                        case LE:
                            if (firstType == Type.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.setgt));
                            instructions.add(new Instruction(Operation.not));
                            first.setType(Type.BOOL);
                            break;
                        case EQ:
                            if (firstType == Type.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.not));
                            first.setType(Type.BOOL);
                            break;
                        case NEQ:
                            if (firstType == Type.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            first.setType(Type.BOOL);
                            break;
                        case PLUS:
                            if (firstType == Type.INT) {
                                if (isGlobal)
                                    startInstructions.add(new Instruction(Operation.addi));
                                else
                                    instructions.add(new Instruction(Operation.addi));
                            } else {
                                if (isGlobal)
                                    startInstructions.add(new Instruction(Operation.addf));
                                else
                                    instructions.add(new Instruction(Operation.addf));
                            }
                            break;
                        case MINUS:
                            if (firstType == Type.INT) {
                                if (isGlobal)
                                    startInstructions.add(new Instruction(Operation.subi));
                                else
                                    instructions.add(new Instruction(Operation.subi));
                            } else {
                                if (isGlobal)
                                    startInstructions.add(new Instruction(Operation.subf));
                                else
                                    instructions.add(new Instruction(Operation.subf));
                            }
                            break;
                        case MUL:
                            if (firstType == Type.INT) {
                                if (isGlobal)
                                    startInstructions.add(new Instruction(Operation.muli));
                                else
                                    instructions.add(new Instruction(Operation.muli));
                            } else {
                                if (isGlobal)
                                    startInstructions.add(new Instruction(Operation.mulf));
                                else
                                    instructions.add(new Instruction(Operation.mulf));
                            }
                            break;
                        case DIV:
                            if (firstType == Type.INT) {
                                if (isGlobal)
                                    startInstructions.add(new Instruction(Operation.divi));
                                else
                                    instructions.add(new Instruction(Operation.divi));

                            } else {
                                if (isGlobal)
                                    startInstructions.add(new Instruction(Operation.divf));
                                else
                                    instructions.add(new Instruction(Operation.divf));
                            }
                            break;
                    }
                } else throw new AnalyzeError(ErrorCode.InvalidType, first.getStartPos());
            }
        } else throw new EmptyStackException();
    }

    private OPGElement analyseOtherExpr(boolean isGlobal) throws CompileError {
        ArrayList<Instruction> changedInstruction=isGlobal?startInstructions:instructions;
        Token token;
        //字面量表达式
        //UINT DOUBLE语义为字面量的值
        if (check(TokenType.UINT_LITERAL))
        {
            token = expect(TokenType.UINT_LITERAL);
            changedInstruction.add(new Instruction(Operation.push, token.getValue()));
            return new OPGElement(Type.INT, token.getStartPos());
        }
        else if (check(TokenType.DOUBLE_LITERAL))
        {
            token = expect(TokenType.DOUBLE_LITERAL);
            changedInstruction.add(new Instruction(Operation.push, Double.doubleToRawLongBits((double) token.getValue())));
            return new OPGElement(Type.DOUBLE, token.getStartPos());
        }
        //字符串语义为对应全局常量的编号
        else if (check(TokenType.STRING_LITERAL)) {
            token = expect(TokenType.STRING_LITERAL);
            changedInstruction.add(new Instruction(Operation.push, (long) globalNo++));
            globals.add(new GlobalDef(1,token.getValueString()));
            return new OPGElement(Type.INT, token.getStartPos());
        }
        else if (check(TokenType.CHAR_LITERAL)) {
            token = expect(TokenType.CHAR_LITERAL);
            changedInstruction.add(new Instruction(Operation.push, (long) (char) token.getValue()));
            return new OPGElement(Type.INT, token.getStartPos());
        }
        //赋值表达式 l_expr -> IDENT  assign_expr -> l_expr '=' expr
        // ident_expr -> IDENT
        else if (check(TokenType.IDENT)) {
            token = expect(TokenType.IDENT);
            //获得标识符在符号表中的位置
            Integer currentIndex = this.hashTable.get(token.getValueString());
            Symbol symbol = null;
            if (currentIndex != null) {
                symbol = this.symbolTable.get(currentIndex);
            }
            //赋值表达式
            if (check(TokenType.ASSIGN))
            {
                if (symbol == null)
                    throw new AnalyzeError(ErrorCode.NotDeclared, token.getStartPos());
                switch (symbol.getSymbolScope()) {
                    case global:
                        instructions.add(new Instruction(Operation.globa, symbol.getOffset()));
                        break;
                    case argument:
                        instructions.add(new Instruction(Operation.arga, symbol.getOffset()));
                        break;
                    case local:
                        instructions.add(new Instruction(Operation.loca, symbol.getOffset()));
                        break;
                }
                Token assign = next();
                OPGElement opgElement = analyseOPGExpr(false);
                changeInitialized(token.getValueString(), token.getStartPos());
                if (opgElement.getType() != symbol.getSymbolType())
                    throw new AnalyzeError(ErrorCode.InvalidAssignment, opgElement.getStartPos());
                instructions.add(new Instruction(Operation.store64));
                return new OPGElement(Type.VOID, assign.getStartPos());
            }
            //函数调用表达式
            else if (nextIf(TokenType.L_PAREN) != null) {
                Type funcReturnType;
                ArrayList<Type> params;
                int callnameOffset = -1;
                if (symbol == null) {
                    switch (token.getValueString()) {
                        case "getint":
                        case "getchar":
                            funcReturnType = Type.INT;
                            params = new ArrayList<>();
                            break;
                        case "getdouble":
                            funcReturnType = Type.DOUBLE;
                            params = new ArrayList<>();
                            break;
                        case "putint":
                        case "putchar":
                        case "putstr":
                            funcReturnType = Type.VOID;
                            params = new ArrayList<Type>() {{
                                add(Type.INT);
                            }};
                            break;
                        case "putdouble":
                            funcReturnType = Type.VOID;
                            params = new ArrayList<Type>() {{
                                add(Type.DOUBLE);
                            }};
                            break;
                        case "putln":
                            funcReturnType = Type.VOID;
                            params = new ArrayList<>();
                            break;

                        default:
                            throw new AnalyzeError(ErrorCode.NotDeclared, token.getStartPos());
                    }
                    globals.add(new GlobalDef(1,token.getValueString()));
                    callnameOffset = globalNo++;
                } else {
                    funcReturnType = symbol.getSymbolType();
                    params = symbol.getParams();
                }

                int stackSize;
                if (funcReturnType == Type.VOID)
                    stackSize = 0;
                else
                    stackSize = 1;
                changedInstruction.add(new Instruction(Operation.stackalloc, stackSize));
                int paramsSize = params.size();
                int i = 0;
                if (check(TokenType.MINUS) || check(TokenType.IDENT) || check(TokenType.UINT_LITERAL) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL) || check(TokenType.L_PAREN)) {
                    OPGElement element = analyseOPGExpr(isGlobal);
                    if (i + 1 > paramsSize || element.getType() != params.get(i++))
                        throw new AnalyzeError(ErrorCode.InvalidType, element.getStartPos());
                    while (nextIf(TokenType.COMMA) != null) {
                        element = analyseOPGExpr(isGlobal);
                        if (i + 1 > paramsSize || element.getType() != params.get(i++))
                            throw new AnalyzeError(ErrorCode.InvalidType, element.getStartPos());
                    }
                }
                expect(TokenType.R_PAREN);
                if (symbol == null)
                    changedInstruction.add(new Instruction(Operation.callname, callnameOffset));
                else
                    changedInstruction.add(new Instruction(Operation.call, symbol.getFuncOffset()));
                return new OPGElement(funcReturnType, token.getStartPos());
            }
            //标识符表达式
            else {
                if (symbol == null)
                    throw new AnalyzeError(ErrorCode.NotDeclared, token.getStartPos());
                switch (symbol.getSymbolScope()) {
                    case global:
                        changedInstruction.add(new Instruction(Operation.globa, symbol.getOffset()));
                        break;
                    case argument:
                        instructions.add(new Instruction(Operation.arga, symbol.getOffset()));
                        break;
                    case local:
                        instructions.add(new Instruction(Operation.loca, symbol.getOffset()));
                        break;
                }
                changedInstruction.add(new Instruction(Operation.load64));
                return new OPGElement(symbol.getSymbolType(), token.getStartPos());
            }
        } else if (check(TokenType.MINUS)) {
            return analyseNegateExpr(isGlobal);
        } else if (check(TokenType.L_PAREN)) {
            expect(TokenType.L_PAREN);
            OPGElement element = analyseOPGExpr(isGlobal);
            expect(TokenType.R_PAREN);
            return element;
        } else
            throw new ExpectedTokenError(Arrays.asList(TokenType.UINT_LITERAL, TokenType.DOUBLE_LITERAL, TokenType.STRING_LITERAL, TokenType.CHAR_LITERAL, TokenType.IDENT, TokenType.MINUS), peek());
    }

    private OPGElement analyseNegateExpr(boolean isGlobal) throws CompileError {
        expect(TokenType.MINUS);
        OPGElement element = analyseOtherExpr(isGlobal);
        if (element.getType() == Type.INT) {
            if (isGlobal)
                startInstructions.add(new Instruction(Operation.negi));
            else
                instructions.add(new Instruction(Operation.negi));
        } else {
            if (isGlobal)
                startInstructions.add(new Instruction(Operation.negf));
            else
                instructions.add(new Instruction(Operation.negf));
        }
        return element;
    }


    private Type analyseType() throws CompileError {
        if (nextIf(TokenType.INT_TY) != null)
            return Type.INT;
        else if (nextIf(TokenType.DOUBLE_TY) != null)
            return Type.DOUBLE;
        else if (nextIf(TokenType.VOID_TY) != null)
            return Type.VOID;
        else {
            List<TokenType> list = Arrays.asList(TokenType.INT_TY, TokenType.DOUBLE_TY, TokenType.VOID_TY);
            throw new ExpectedTokenError(list, peek());
        }

    }

}
