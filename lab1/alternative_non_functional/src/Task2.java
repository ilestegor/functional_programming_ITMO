public class Task2 {

    public int solve(int x, int y, int maxFibNum){
        int sum = 0;

        do {
            if (y % 2 == 0) {
                sum += y;
            }
            int temp = x + y;
            x = y;
            y = temp;
        } while (y < maxFibNum);

       return sum;
    }
}
