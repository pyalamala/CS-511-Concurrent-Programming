public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        // TODO: Implement me!
        for(int i = 0; i < this.interval.getY() - this.interval.getX(); i++){
            buffer[i + offset] = content.charAt(this.interval.getX() + i);
        }
    }
}