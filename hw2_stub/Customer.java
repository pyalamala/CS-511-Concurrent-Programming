import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CountDownLatch;
import java.util.ArrayList;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;
    private CountDownLatch doneSignal;
    
    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery, CountDownLatch l) {
        // TODO
        this.bakery = bakery;
        this.rnd = new Random();
        this.shoppingCart = new ArrayList<BreadType>();
        this.shopTime = rnd.nextInt(100);
        this.checkoutTime = rnd.nextInt(100);
        this.doneSignal = l;
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO
        try {
            this.fillShoppingCart();
            this.bakery.shelves.acquire();
            System.out.println(this.toString() + " is shopping");
            Thread.sleep(shopTime);

            for(BreadType bread: this.shoppingCart){
                if (bread == BreadType.RYE){
                    this.bakery.ryeShelf.acquire();
                    //System.out.println(this.toString()+ " taking rye bread");
                    this.bakery.takeBread(bread);
                    //System.out.println(this.toString()+ " done taking rye bread");
                    this.bakery.ryeShelf.release();
                }
                else if (bread == BreadType.SOURDOUGH){
                    this.bakery.doughShelf.acquire();
                    //System.out.println(this.toString()+ " taking sourdough bread");
                    this.bakery.takeBread(bread);
                    //System.out.println(this.toString()+ " done taking sourdough bread");
                    this.bakery.doughShelf.release();
                }
                else if (bread == BreadType.WONDER){
                    this.bakery.wonderShelf.acquire();
                    //System.out.println(this.toString()+ " taking wonder bread");
                    this.bakery.takeBread(bread);
                    //System.out.println(this.toString()+ " done taking wonder bread");
                    this.bakery.wonderShelf.release();
                }
            }

            System.out.println(this.toString()+ " done shopping.");
            this.bakery.shelves.release();
            this.bakery.cashiers.acquire();
            System.out.println(this.toString() + " went to cashier");
            this.bakery.sales_mutex.acquire();
            System.out.println(this.toString() + " is checking out");
            Thread.sleep(checkoutTime);
            this.bakery.addSales(this.getItemsValue());
            System.out.println(this.toString() + " done checking out");
            this.bakery.cashiers.release();
            this.bakery.sales_mutex.release();
            this.doneSignal.countDown();
        }
        catch (Exception e){
            e.printStackTrace();
        }
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}
