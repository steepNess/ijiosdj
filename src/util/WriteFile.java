package util;

import analyser.GlobalDef;
import instruction.FunctionInstruction;
import instruction.Instruction;
import instruction.Operation;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

public class WriteFile {

    public static ArrayList<ArrayList<Instruction>> functions = new ArrayList<>();

    public static void outFile(String fileName, ArrayList<GlobalDef> globals, ArrayList<Instruction> instructions,
                               ArrayList<Instruction> startInstructions) throws IOException {
        FileOutputStream fos = new FileOutputStream(new File(fileName));
        //魔数
        fos.write(intToByte(0x72303b3e));
        //版本号
        fos.write(intToByte(1));
        //globals.count
        fos.write(intToByte(globals.size()));
        //globals
        for(GlobalDef global:globals)
        {
            fos.write(global.is_const);
            fos.write(global.array_count);
            fos.write(global.array_items);
        }
        functions.add(startInstructions);
        cutFunction(instructions);
        fos.write(intToByte(functions.size()));  //functions.count
        for (ArrayList<Instruction> funcInstructions : functions) {
            for (Instruction instruction : funcInstructions) {
                if (instruction.getOperation() == Operation.func) {
                    FunctionInstruction functionInstruction = (FunctionInstruction) instruction;
                    fos.write(intToByte(functionInstruction.getOffset()));
                    fos.write(intToByte(functionInstruction.getRetSlots()));
                    fos.write(intToByte(functionInstruction.getParamSlots()));
                    fos.write(intToByte(functionInstruction.getLocalSlots()));
                    fos.write(intToByte(funcInstructions.size() - 1));
                } else if (instruction.getX() == null)
                    fos.write(instruction.getOperation().getValue());
                else {
                    fos.write(instruction.getOperation().getValue());
                    if (instruction.getOperation() == Operation.push)
                        fos.write(longToByte((long) instruction.getX()));   //push占8字节
                    else
                        fos.write(intToByte((int) instruction.getX()));   //int占4字节
                }
            }
        }

        fos.close();
    }

    public static byte[] longToByte(long val) {
        byte[] b = new byte[8];
        b[7] = (byte) (val & 0xff);
        b[6] = (byte) ((val >> 8) & 0xff);
        b[5] = (byte) ((val >> 16) & 0xff);
        b[4] = (byte) ((val >> 24) & 0xff);
        b[3] = (byte) ((val >> 32) & 0xff);
        b[2] = (byte) ((val >> 40) & 0xff);
        b[1] = (byte) ((val >> 48) & 0xff);
        b[0] = (byte) ((val >> 56) & 0xff);
        return b;
    }

    public static byte[] intToByte(int val) {
        byte[] b = new byte[4];
        b[3] = (byte) (val & 0xff);
        b[2] = (byte) ((val >> 8) & 0xff);
        b[1] = (byte) ((val >> 16) & 0xff);
        b[0] = (byte) ((val >> 24) & 0xff);
        return b;
    }

    public static void cutFunction(ArrayList<Instruction> instructions) {
        int first = 0;
        for (int i = 1; i < instructions.size(); i++) {
            if (instructions.get(i).getOperation() == Operation.func) {
                functions.add(new ArrayList<>(instructions.subList(first, i)));
                first = i;
            }
        }
        functions.add(new ArrayList<>(instructions.subList(first, instructions.size())));
    }
}
