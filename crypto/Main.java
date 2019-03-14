import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class Main {

    public static void main (String[] args){
        System.out.println("heapsize is: " + Runtime.getRuntime().totalMemory()/1000000 + "MB");
        CyclicGenerator cg = new CyclicGenerator(10000000);
        System.out.println("freeheap is: " + Runtime.getRuntime().freeMemory()/1000000 + "MB");
        System.out.println("Generate group");
        cg.generate();
        System.out.println("freeheap is: " + Runtime.getRuntime().freeMemory()/1000000 + "MB");
        System.out.println("p: " + cg.getP());
        System.out.println("q: " + cg.getQ());
        System.out.println("g: " + cg.getG());
        long[] arr = cg.getGroup();
        System.out.println(arr.length);
        Set<Long> s = new HashSet<>();
        for(int i = 0; i < arr.length; i++){
            s.add(arr[i]);
        }
        System.out.println("has no dup?: " + (arr.length == s.size() ? "true" : "false"));

    }


}
