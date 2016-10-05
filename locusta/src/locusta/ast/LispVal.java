package locusta.ast;

import locusta.Environment;


/**
 * Created by chenda on 16/9/29.
 */
public abstract class LispVal {
    public abstract Object eval(Environment env);
    public abstract String toString();
}
