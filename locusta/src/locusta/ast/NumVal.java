package locusta.ast;

import locusta.Environment;

/**
 * Created by chenda on 16/9/29.
 */
public class NumVal extends LispVal {

    public String numStr;
    public double value;

    public NumVal(String numStr){
        this.numStr = numStr;
        this. value = Double.parseDouble(numStr);
    }

    public Double eval(Environment env) {
        return value;
    }

    @Override
    public String toString() {
        return numStr;
    }
}
