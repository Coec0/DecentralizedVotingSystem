import java.util.Set;

public class FindCurves {
    private static Curve ellipticCurve;

    private FindCurves(Curve ellipticCurve){
        this.ellipticCurve = ellipticCurve;
    }

    /***
     *
     * @param x the x-value
     * @param ellipticCurve The curve
     * @return The RHS value mod p. Result will always be integer if x, a, b and p are.
     */
    private static double calculateRHS(int x, Curve ellipticCurve){
        double xd = Integer.valueOf(x).doubleValue();
        return ((Math.pow(xd,3)+Math.multiplyExact(ellipticCurve.getA(),x)+ellipticCurve.getB())%(ellipticCurve.getP()));
    }

    /***
     * @param value
     * @return whether or not the value is an integer
     */
    private boolean isInteger(double value){

        return (value % 1 == 0);

    }

    /***
     * @param number The number to be squarerooted
     * @return the squareroot as a double, modulo the modulous of the curve.
     */
    private double calculateModSqrt(double number, Curve ellipticCurve){
        return (Math.sqrt(number)%(ellipticCurve.getP()));
    }

    /***
     * Checks if a point should be on the curve from the x side,
     * and in that case adds the point to the curve.
     * @param x this x-value

     */
    private void addPointtoCurve(int x, Curve ellipticCurve){
        double RHS = calculateRHS(x,ellipticCurve);
        double y = calculateModSqrt(RHS, ellipticCurve);
        if(isInteger(y)){
            int yint = new Double(y).intValue();
            ellipticCurve.addPoint(new CurvePoint(x,yint));
            ellipticCurve.addPoint(new CurvePoint(x,ellipticCurve.getP()-yint));
        }
    }

    /***
     * Runs the algorithm for checking a points for all possible x and y-values
     * that is all [0,p)
     */
    private void findAllPointsOnCurve(){
        int p = ellipticCurve.getP();
        for(int x = 0; x < p; x++){
            addPointtoCurve(x, ellipticCurve);
            double RHS = calculateRHS(x, ellipticCurve);
            for(int y = 0; y < p; y++){
                if((Math.pow(y,2)%p)==RHS){
                    ellipticCurve.addPoint(new CurvePoint(x,y));
                }
            }

        }
    }

    /***
     *
     * @return all points on the curve
     */
    public Set<CurvePoint> getPointsOnCurve(){
        return ellipticCurve.getPointsOnCurve();
    }

    /***
     * Finds and prints all points on a specified curve
     *
     */
    public static void main(String[] args){
        FindCurves findCurves = new FindCurves(new Curve(1,1, 109));
        findCurves.findAllPointsOnCurve();
        System.out.print(findCurves.getPointsOnCurve());

    }

}
