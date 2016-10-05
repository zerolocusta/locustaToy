package locusta;

import locusta.ast.LispVal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Created by chenda on 16/9/29.
 */
public class Environment {

    public static List<HashMap<String, LispVal>> env;

    public Environment(){
        env = new ArrayList<>();
    }

    public void push_frame(HashMap<String, LispVal> frame){
        env.add(frame);
    }

    public HashMap<String, LispVal> pop_frame(){
        HashMap<String, LispVal> frame = env.get(0);
        env.remove(0);
        return frame;
    }

    public HashMap<String, LispVal> peek_frame(){
        return env.get(0);
    }

    public LispVal lookup_variable(String name){
        int length = env.size() - 1;
        for(; length > 0 ; length--){
            HashMap<String, LispVal> frame = env.get(length);
            if(frame.containsKey(name))
                return frame.get(name);
        }
        return null;
    }

}
