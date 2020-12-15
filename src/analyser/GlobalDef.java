package analyser;

public class GlobalDef {
    public int is_const;
    public byte[] array_count;   //变量占据字节数
    public byte[] array_items;    //变量各字节的值

    public GlobalDef(int is_const,String global) {
        this.is_const = is_const;
        this.array_count = intToByte(global.length());
        this.array_items =global.getBytes();
    }
    public GlobalDef(int is_const) {
        this.is_const = is_const;
        //global.value.count 4 slot
        this.array_count=intToByte(8);
        //global.value.items 8 slot
        this.array_items=longToByte(0L);
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

}
