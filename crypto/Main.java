import java.util.Arrays;

public class Main {

    public static void main (String[] args){
        CyclicGenerator cg = new CyclicGenerator(1,10000);
        cg.generate();
        System.out.println(Arrays.toString(cg.getGroup()));
        System.out.println("p: " + cg.getP());
        System.out.println("q: " + cg.getQ());

    }


}
