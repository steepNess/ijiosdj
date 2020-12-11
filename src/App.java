import analyser.Analyser;
import instruction.Operation;
import tokenizer.StringIter;
import tokenizer.Token;
import tokenizer.TokenType;
import tokenizer.Tokenizer;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class App {
    public static void main(String[] args) throws Exception{
        //InputStream input = new FileInputStream("input.txt");
        Scanner scanner=new Scanner(new File(args[0]));
        StringIter it=new StringIter(scanner);
        Tokenizer tokenizer=new Tokenizer(it);
        Analyser analyser=new Analyser(tokenizer);
        //DataOutputStream output = new DataOutputStream(new FileOutputStream(new File("output.txt")));
        analyser.analyse(args[1]);
    }
}
