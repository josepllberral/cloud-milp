public class Sleep1 implements Runnable {

    Thread th;

    public void run() {
        for (int i = 0; i <= 10; i++) {

            System.out.println(Thread.currentThread().getName() + " " + i);
            try {
                Thread.currentThread().sleep(1000);
            } catch (Exception e) {
                System.out.println(e);
            }
        }
    }

    public static void main(String[] args) throws Exception {
        Thread th = new Thread(new Sleep1());
        th.start();
    }
}