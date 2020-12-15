package analyser;

import symbol.Type;
import util.Pos;

public class OPGElement {
    private Type type;
    private Pos startPos;

    public OPGElement(Type type, Pos startPos) {
        this.type = type;
        this.startPos = startPos;
    }

    public Type getType() {
        return type;
    }

    public Pos getStartPos() {
        return startPos;
    }

    public void setType(Type type) {
        this.type = type;
    }
}
