package locusta.ast;

import locusta.Environment;

/**
 * Created by chenda on 16/9/29.
 */
public class SymbolVal extends LispVal{
    private String name;

    public SymbolVal(String name){
        this.name = name;
    }

    public LispVal eval(Environment env) {
        return env.lookup_variable(this.name);
    }

    @Override
    public String toString() {
        return name;
    }
}
