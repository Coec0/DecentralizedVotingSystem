import java.util.*;

public class CyclicGenerator{
    private int p;
    private int q;
    private int g;
    private int[] group;
    private List<Integer> primesList = new ArrayList<>();
    private Deque<Integer> primes;
    private int min;
    private int max;

    public CyclicGenerator(int min, int max){
        System.out.println("Starting primesgenerator");
        generatePrimes(min,max);
        System.out.println("End primesgenerator");
        Collections.shuffle(primesList);
        primes = new ArrayDeque<>(primesList);
        this.min = min;
        this.max = max;
    }


    public void generate(){
        do{
            p = findP();
            g = findG(p,q);
        }while(g == -1);

        group = new int[q];
        generateGroup();
    }


    private void generateGroup(){
        for(int a = 0; a < q; a++){
            group[a] = (int) Math.pow(g,a) % p;
        }
    }

    private int findP(){
       this.q = getRandomPrime();
       int p = calculateP(q);
       if(isPrime(p)){
           return p;
       }else{
           return findP();
       }
    }

    private void generatePrimes(int min, int max){
        for(int i = min; i < max; i++){
            if(isPrime(i)){
                primesList.add(i);
            }
        }
    }

    private synchronized int getRandomPrime(){
        return primes.remove();
    }

    private int calculateP(int q){
        return 2*q + 1;
    }

    private int findG(int p, int q){
        int x;
        for(int g = 2; g < p; g++){
            x = (int) (Math.pow(g, q) % p);
            if(x == 1){
                return g;
            }
        }
        return -1;
    }

    private boolean isPrime(int number){
        if (number == 2 || number == 3) {
            return true;
        }
        if (number % 2 == 0) {
            return false;
        }
        int sqrt = (int) Math.sqrt(number) + 1;
        for (int i = 3; i < sqrt; i += 2) {
            if (number % i == 0) {
                return false;
            }
        }
        return true;
    }

    public int getP() {
        return p;
    }

    public int getQ() {
        return q;
    }

    public int getG() {
        return g;
    }

    public int[] getGroup() {
        return group;
    }

    public List<Integer> getPrimes() {
        return primesList;
    }

}
