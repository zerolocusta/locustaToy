package locusta.ast;

import locusta.Environment;

/**
 * Created by chenda on 16/10/1.
 */
public class BoolVal extends LispVal {
    private boolean value;

    public BoolVal(boolean value){
        this.value = value;
    }

    public Boolean eval(Environment env) {
        return value;
    }

    @Override
    public String toString() {
        if (value == true)
            return "#t";
        return "#f";
    }
}
