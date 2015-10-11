public class ProblemaII{
    
    public static void main (String[] args) {
        boolean fib = false && (fibonacci(100000) > 1);
        System.out.println(fib);
        System.out.println(fibonacci(100000));
    }

    public static int fibonacci(int n) {
        if(n < 2)
            return 1;
        return fibonacci(n-1) + fibonacci(n-2);
    }
}