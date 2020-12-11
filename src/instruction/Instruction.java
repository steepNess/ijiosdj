package instruction;

public class Instruction {
    //操作指令
    private Operation operation;
    //参数
    private Object x =null;

    public Instruction(Operation operation) {
        this.operation = operation;
    }

    public Instruction(Operation operation, Object x) {
        this.operation = operation;
        this.x = x;
    }

    public Operation getOperation() {
        return operation;
    }

    public Object getX() {
        return x;
    }

    public void setX(Object x) {
        this.x = x;
    }

    @Override
    public String toString() {
        return operation +
                "(" + x +
                ')';
    }
}
