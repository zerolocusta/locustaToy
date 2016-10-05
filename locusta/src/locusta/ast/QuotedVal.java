package locusta.ast;

import locusta.Environment;

/**
 * Created by chenda on 2016/10/3.
 */
public class QuotedVal extends LispVal {
    private LispVal quotedVal;

    public QuotedVal(LispVal quotedVal){
        this.quotedVal = quotedVal;
    }

    public LispVal eval(Environment env) {
        return quotedVal;
    }

    @Override
    public String toString() {
        return quotedVal.toString();
    }
}
