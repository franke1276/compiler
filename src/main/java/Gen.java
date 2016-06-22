import org.apache.bcel.Constants;
import org.apache.bcel.generic.*;

import java.io.IOException;

import static org.apache.bcel.Constants.ACC_PUBLIC;
import static org.apache.bcel.Constants.ACC_STATIC;
import static org.apache.bcel.Constants.ACC_SUPER;

public class Gen {
    public static MethodGen declareMethode(InstructionList il, Type returnType,String[] argNames, Type[] argType, String methodName) {
        return new MethodGen(ACC_STATIC | ACC_PUBLIC, // access flags
                returnType,               // return type
                argType ,
                argNames,
                methodName, "HelloWorld",
                il, cp);
    }



    public static void buildMain(InstructionList il, MethodGen mg) {
        LocalVariableGen  lg = mg.addLocalVariable("x", Type.INT, null, null);
        int x = lg.getIndex();
        il.append(new PUSH(cp, 1));
        lg.setStart(il.append(new ISTORE(x)));

        il.append(factory.createFieldAccess("java.lang.System", "out", p_stream, Constants.GETSTATIC));
        il.append(new ILOAD(x));


        il.append(factory.createInvoke("java.io.PrintStream", "println",
                Type.VOID, new Type[] { Type.INT },
                Constants.INVOKEVIRTUAL));

        il.append(InstructionConstants.RETURN);

    }

    private static ObjectType p_stream = new ObjectType("java.io.PrintStream");
    private static ClassGen cg = new ClassGen("HelloWorld", "java.lang.Object",
            "<generated>", ACC_PUBLIC | ACC_SUPER, null);

    private static ConstantPoolGen cp = cg.getConstantPool(); // cg creates constant pool
    private static  InstructionFactory factory = new InstructionFactory(cg);

    public static void main(String[] args) {
        InstructionList il = new InstructionList();

        MethodGen method1 = declareMethode(il, Type.INT,
                new String[] { "arg" },
                new Type[] { Type.INT },
                "methode1");

        il.append(new PUSH(cp, 1));
        il.append(InstructionConstants.IRETURN);
        method1.setMaxStack();
        cg.addMethod(method1.getMethod());
        il.dispose();

        MethodGen mainMethod = declareMethode(il, Type.VOID,
                new String[] { "argv" },
                new Type[] { new ArrayType(Type.STRING, 1) },
                "main");
        buildMain(il, mainMethod);
        mainMethod.setMaxStack();
        cg.addMethod(mainMethod.getMethod());
        il.dispose();

        cg.addEmptyConstructor(ACC_PUBLIC);

        try {
            cg.getJavaClass().dump("HelloWorld.class");
        } catch (IOException e) {
            System.err.println(e);
        }
    }
}
