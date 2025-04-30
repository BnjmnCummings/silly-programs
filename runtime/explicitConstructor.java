class C {
    public final int n;
    
    public C() {
        n = 1;
    }
    
    public int getN() {
        return n;
    }
}

class D extends C {
    public final int n;;

    public D() {
        n = 2;
    }

    public int getN() {
        return n;
    }
}

/**
 * - surely with explicit constructos calls this won't happen right...
 * - new A() is never invoked so the field n from A is never initialized
 * - the field n from B is initialized to 2
 * 
 */
class explicitConstructorProgram {
    public static void main(String[] args) {
        D d = new D();
        C c = d;

        System.out.println(c.getN()); // Output: 2
        System.out.println(c.n); // Output: 1 WHY???
        System.out.println("what");
    }
}

/*
 * Why is this happening?
 * - The constructor of the superclass is called before the constructor of the subclass.
 *      ie. D() actually calls C() under the hood before executing the body of D().
 * - So we've not gotten away with anything.
 */
