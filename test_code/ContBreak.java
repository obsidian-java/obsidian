public class ContBreak {
    public static void main(String[] args) {
        f1(); 
        f2();
        f3();
    }
    public static void f1() { 
        for (int i=0; i<10; i++) {
            if (i % 2 == 0) {
                continue;
            }
            System.out.println(i);
        }
    }
    // same as above
    public static void f2() {
        for (int i=0; i<10; i++) {
            body : {
                if (i % 2 == 0) {
                    break body; // break out of body only
                }
                System.out.println(i);

            }
        }
    }

    public static void f3() {
        loop: for (int i=0; i<10; i++) {
            if (i % 2 == 0) {
                continue loop; // continue a specific loop with name loop
            }
            System.out.println(i);            
        }
    }    
}