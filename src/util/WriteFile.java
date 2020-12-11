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
        FileOutputStream out = new FileOutputStream(new File(fileName));
        //魔数
        out.write(intToByte(0x72303b3e));
        //版本号
        out.write(intToByte(1));
        //globals.count
        out.write(intToByte(globals.size()));
        //globals
        /*
        for (String global : globals) {
            switch (global) {
                //全局变量
                case "0":
                    //global.isConst 1 slot
                    out.write(0);
                    //global.value.count 4 slot
                    out.write(intToByte(8));
                    //global.value.items 8 slot
                    out.write(longToByte(0L));
                    break;
                //全局常量
                case "1":
                    out.write(1);
                    out.write(intToByte(8));
                    out.write(longToByte(0L));
                    break;
                //全局变量的值
                default:
                    out.write(1);
                    out.write(intToByte(global.length()));
                    out.write(global.getBytes());
            }
        }*/
        for(GlobalDef global:globals)
        {
            out.write(global.is_const);
            out.write(global.array_count);
            out.write(global.array_items);
        }
        functions.add(startInstructions);
        cutFunction(instructions);
        out.write(intToByte(functions.size()));
        for (ArrayList<Instruction> funcInstructions : functions) {
            for (Instruction instruction : funcInstructions) {
                if (instruction.getOperation() == Operation.func) {
                    FunctionInstruction functionInstruction = (FunctionInstruction) instruction;
                    out.write(intToByte(functionInstruction.getOffset()));
                    out.write(intToByte(functionInstruction.getReturnCount()));
                    out.write(intToByte(functionInstruction.getParamCount()));
                    out.write(intToByte(functionInstruction.getLocalCount()));
                    out.write(intToByte(funcInstructions.size() - 1));
                } else if (instruction.getX() == null)
                    out.write(instruction.getOperation().getValue());
                else {
                    out.write(instruction.getOperation().getValue());
                    if (instruction.getOperation() == Operation.push)
                        out.write(longToByte((long) instruction.getX()));
                    else
                        out.write(intToByte((int) instruction.getX()));
                }
            }
        }

        out.close();
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
