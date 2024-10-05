import java.math.BigInteger;
import java.util.HashSet;

public class Task29 {

    public int solve(int a, int b){
        HashSet<BigInteger> uniqueSet = new HashSet<>();
        for (int i = a; i <= b; i++) {
            for (int j = a; j <= b; j++) {
                uniqueSet.add(BigInteger.valueOf(i).pow(j));
            }
        }
        return uniqueSet.size();
    }
}
