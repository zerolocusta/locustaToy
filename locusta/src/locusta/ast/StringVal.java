package locusta.ast;

import locusta.Environment;

/**
 * Created by chenda on 16/9/29.
 */
public class StringVal extends LispVal {

    private String literal;

    public StringVal(String literal){
        this.literal = literal;
    }

    public String eval(Environment env) {
        return literal;
    }

    @Override
    public String toString() {
        return '"'+ this.literal + '"';
    }
}
