public class CurvePoint implements Comparable {
    int x;
    int y;

    public CurvePoint(int x, int y){
        this.x = x;
        this.y = y;
    }

    public void setX(int x) {
        this.x = x;
    }

    public void setY(int y) {
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public String toString(){
        return "("+x+","+y+")";
    }

    public int compareTo(CurvePoint otherPoint) {
        if(getX() > otherPoint.getX()||(getX()==otherPoint.getX())&&(getY()>otherPoint.getY()))
            return 1;
        if((getX()==otherPoint.getX())&&(getY()==otherPoint.getY()))
            return 0;
        else{
            return -1;
        }
    }

    @Override
    public int compareTo(Object o) {
        if(o == null){
            throw new NullPointerException("There is no object to compare.");
        }
        if(!o.getClass().equals(CurvePoint.class)){
            throw new ClassCastException("The object you are trying to compare is not of class CurvePoint.");
        }
        return compareTo((CurvePoint) o);
    }
}
