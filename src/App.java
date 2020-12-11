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
        //InputStream input = new FileInputStream("src/input.txt");
        InputStream input = new FileInputStream(args[0]);
        Scanner scanner=new Scanner(input);
        StringIter it=new StringIter(scanner);
        Tokenizer tokenizer=new Tokenizer(it);
        Analyser analyser=new Analyser(tokenizer);
        //String outputFile="src/output.txt";
        String outputFile=args[1];
        analyser.analyse(outputFile);
    }
}
