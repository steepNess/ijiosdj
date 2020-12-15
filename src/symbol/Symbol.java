package symbol;

import java.util.ArrayList;

public class Symbol {
    private String name;
    private boolean isConstant = false;
    private boolean isInitialized = false;
    private boolean isFunction = false;
    private Type type;
    private ArrayList<Type> params = null;
    private Scope scope;
    private int offset;
    private int funcOffset;
    private Integer conflictChain;

    public Symbol(String name, boolean isFunction, Scope scope, int offset, int funcOffset) {
        this.name = name;
        this.isFunction = isFunction;
        this.scope = scope;
        this.offset = offset;
        this.funcOffset=funcOffset;
        this.params=new ArrayList<>();
    }

    public Symbol(String name, boolean isConstant, boolean isInitialized, Type type, Scope scope, int offset) {
        this.name = name;
        this.isConstant = isConstant;
        this.isInitialized = isInitialized;
        this.type = type;
        this.scope = scope;
        this.offset = offset;
    }

    public Symbol(String name, boolean isConstant, boolean isInitialized, Type type, int chain, Scope scope, int offset) {
        this.name = name;
        this.isConstant = isConstant;
        this.isInitialized = isInitialized;
        this.type = type;
        this.scope = scope;
        this.offset = offset;
        this.conflictChain = chain;
    }

    public int getFuncOffset() {
        return funcOffset;
    }

    public void setFuncOffset(int funcOffset) {
        this.funcOffset = funcOffset;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public ArrayList<Type> getParams() {
        return params;
    }

    public void setParams(ArrayList<Type> params) {
        this.params = params;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public boolean isConstant() {
        return isConstant;
    }

    public void setConstant(boolean constant) {
        isConstant = constant;
    }

    public boolean isInitialized() {
        return isInitialized;
    }

    public void setInitialized(boolean initialized) {
        isInitialized = initialized;
    }

    public Type getSymbolType() {
        return type;
    }

    public void setSymbolType(Type type) {
        this.type = type;
    }

    public Integer getConflictChain() {
        return conflictChain;
    }

    public void setConflictChain(Integer conflictChain) {
        this.conflictChain = conflictChain;
    }

    public boolean isFunction() {
        return isFunction;
    }

    public void setFunction(boolean function) {
        isFunction = function;
    }

    public Scope getSymbolScope() {
        return scope;
    }

    public void setStorageType(Scope scope) {
        this.scope = scope;
    }
}
