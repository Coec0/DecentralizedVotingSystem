import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class Main {

    public static void main (String[] args){
        CyclicGenerator cg = new CyclicGenerator(100000,10000000);
        cg.generate();
        System.out.println(Arrays.toString(cg.getGroup()));
        System.out.println("p: " + cg.getP());
        System.out.println("q: " + cg.getQ());
        System.out.println("g: " + cg.getG());
        long[] arr = cg.getGroup();
        Set<Long> s = new HashSet<Long>();
        for(int i = 0; i < arr.length; i++){
            s.add(arr[i]);
        }
        System.out.println("has no dup?: " + (arr.length == s.size() ? "true" : "false"));

    }


}
