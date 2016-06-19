import org.apache.bcel.Constants;
import org.apache.bcel.generic.*;

import java.io.IOException;

import static org.apache.bcel.Constants.ACC_PUBLIC;
import static org.apache.bcel.Constants.ACC_STATIC;
import static org.apache.bcel.Constants.ACC_SUPER;

public class Gen {
    public static void main(String[] args) {
        ClassGen cg = new ClassGen("HelloWorld", "java.lang.Object",
                "<generated>", ACC_PUBLIC | ACC_SUPER, null);
        ConstantPoolGen cp = cg.getConstantPool(); // cg creates constant pool
        InstructionList il = new InstructionList();

        MethodGen mg = new MethodGen(ACC_STATIC | ACC_PUBLIC, // access flags
                Type.VOID,               // return type
                new Type[] {             // argument types
                        new ArrayType(Type.STRING, 1) },
                new String[] { "argv" }, // arg names
                "main", "HelloWorld",    // method, class
                il, cp);
        InstructionFactory factory = new InstructionFactory(cg);

        ObjectType p_stream = new ObjectType("java.io.PrintStream");


        LocalVariableGen  lg = mg.addLocalVariable("x", Type.INT, null, null);
        int x = lg.getIndex();
        il.append(new PUSH(cp, 9));
        lg.setStart(il.append(new ISTORE(x)));

        il.append(factory.createFieldAccess("java.lang.System", "out", p_stream, Constants.GETSTATIC));
        il.append(new ILOAD(x));


        il.append(factory.createInvoke("java.io.PrintStream", "println",
                Type.VOID, new Type[] { Type.INT },
                Constants.INVOKEVIRTUAL));

        il.append(InstructionConstants.RETURN);


        mg.setMaxStack();
        cg.addMethod(mg.getMethod());
        il.dispose(); // Allow instruction handles to be reused
        cg.addEmptyConstructor(ACC_PUBLIC);

        try {
            cg.getJavaClass().dump("HelloWorld.class");
        } catch (IOException e) {
            System.err.println(e);
        }
    }
}
