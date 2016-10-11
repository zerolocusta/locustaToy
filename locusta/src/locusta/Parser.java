package locusta;

import locusta.ast.LispVal;
import locusta.ast.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import static locusta.Token.*;

/**
 * Created by chenda on 16/9/29.
 */
public class Parser {

    private static BufferedReader bfr;
    private static String identifityStr;
    private static int curTok;
    private static double NumVal;

    public static String getIdentifityStr() {
        return identifityStr;
    }

    public static double getNumVal() {
        return NumVal;
    }

    public static void main(String[] args) throws IOException {
        Parser parser = new Parser();
        while(true){
            System.out.println(parser.parseExpr());
        }
    }

    public Parser() {
        bfr = new BufferedReader(new InputStreamReader(System.in));
    }

    public char getChar() throws IOException {
        return (char) bfr.read();
    }

    public int getTok() throws IOException {
        char lastChar = ' ';
        identifityStr = "";

        while (Character.isSpaceChar(lastChar))

            lastChar = getChar();

        if (Character.isLetter(lastChar)) {
            do {
                identifityStr += lastChar;
            } while (Character.isLetterOrDigit(lastChar = getChar()));
            return tok_symbol;
        }

        if (lastChar == '"') {
            while ((lastChar = getChar()) != '"')
                identifityStr += lastChar;
            if (lastChar == '"') {
                lastChar = getChar();
                return tok_string;
            }
        }

        if (Character.isDigit(lastChar)){
            do{
                identifityStr += lastChar;
                lastChar = getChar();
            }while(Character.isDigit(lastChar) || lastChar == '.');
            NumVal = Double.parseDouble(identifityStr);
            return tok_number;
        }

        int thisChar = lastChar;
        lastChar = getChar();
        return thisChar;
    }

    public int getNextTok() throws IOException {
        curTok = getTok();
        return curTok;
    }


    public SymbolVal parseSymbol(){
        String symbol = identifityStr;
        return new SymbolVal(symbol);
    }

    public NumVal parseNumber(){
        return new NumVal(identifityStr);
    }

    public StringVal parseString(){
        return new StringVal(identifityStr);
    }

    public LispVal parseQuoted() throws IOException {
        return new QuotedVal(parseExpr());
    }

    public LispVal parseExpr() throws IOException {

        switch (getNextTok()){
            case tok_symbol:
                return parseSymbol();
            case tok_number:
                return parseNumber();
            case tok_string:
                return parseString();
            case '\'':
                return parseQuoted();
        }
        return null;
    }



}
