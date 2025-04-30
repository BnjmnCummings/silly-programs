class A {
    public final int n = 1;
    
    public int getN() {
        return n;
    }
}

class B extends A {
    public final int n = 2;

    public int getN() {
        return n;
    }
}

/*
 * - a.getN() returns 2, because it calls the method from class B (a references the object b)
 * - However, a.n returns 1, because field access at compile time is done 
 *  based on the declared type of the reference (in this case A).
 * - This is another good reason to keep fields private and use getters/setters :)
 */
class program {
    public static void main(String[] args) {
        B b = new B();
        A a = b;

        System.out.println(a.getN()); // Output: 2
        System.out.println(a.n); // Output: 1 WHY???
    }
}

/* 
 * Field Hiding
 * - In Java, if a subclass defines a field with the same name as a field in its superclass,
 *  we don't override the field, we hide it.
 * - This means that the subclass field is a completely separate field from the superclass field.
 * - So, inside the instance b, we have fields n (from class A) and n (from class B).
 * - now, n (from class B) usually hides n (from class A) when we access it through a reference of type B.
 * - However, when we access it through a reference of type A, we are accessing the field n (from class A).
 */
