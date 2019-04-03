import java.util.*;

public class Curve {
    private Set<CurvePoint> pointsOnCurve;
    private int A;
    private int B;
    private int p;

    public Curve(int A, int B, int p){
        pointsOnCurve = new TreeSet<CurvePoint>();
        this.A = A;
        this.B = B;
        this.p = p;
    }

    public void addPoint(CurvePoint curvePoint){
        pointsOnCurve.add(curvePoint);
    }

    public int getA(){
        return A;
    }

    public int getB() {
        return B;
    }

    public int getP(){
        return p;
    }

    public Set<CurvePoint> getPointsOnCurve() {
        return pointsOnCurve;
    }
}
