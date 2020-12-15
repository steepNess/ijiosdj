package instruction;

import java.util.List;

public class FunctionInstruction extends Instruction{
    //局部变量个数
    private int localSlots;
    //如果返回值是int，则为1；是double，则为2；是void，则为0
    private int retSlots;
    //参数列表大小
    private int paramSlots;
    //该函数在全局符号表内的位置
    private int offset;
    //函数内指令
    private List<Instruction> body;

    public FunctionInstruction(Operation operation) {
        super(operation);
    }

    public FunctionInstruction(Operation operation, int localSlots, int retSlots, int paramSlots, int offset) {
        super(operation);
        this.localSlots = localSlots;
        this.retSlots = retSlots;
        this.paramSlots = paramSlots;
        this.offset = offset;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public int getLocalSlots() {
        return localSlots;
    }

    public void setLocalSlots(int localSlots) {
        this.localSlots = localSlots;
    }

    public int getRetSlots() {
        return retSlots;
    }

    public void setRetSlots(int retSlots) {
        this.retSlots = retSlots;
    }

    public int getParamSlots() {
        return paramSlots;
    }

    public void setParamSlots(int paramSlots) {
        this.paramSlots = paramSlots;
    }

    @Override
    public String toString() {
        return  getOperation()+"["+offset+
                "] " + localSlots +
                " " + paramSlots +
                "->" + retSlots;
    }
}
