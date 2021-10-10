
/**
 * Write a description of insertSort here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import java.util.Scanner;

public class insertSort {
    public static void main(String args[]){
        int ptrA = 0;
        int ptrB = 0;
        Scanner sc = new Scanner(System.in);
        
        System.out.println("Enter the number of elements: ");
        int size = sc.nextInt();
        
        int[] myArray = new int[size];
        // must leave a space! in your input
        System.out.println("Input your elements ");
        for (int i = 0; i < size; i ++ ){
            myArray[i] = sc.nextInt();
        }
        
        for (int i = 0; i < myArray.length; i++){
            ptrA = myArray[i];
            for (int j = i - 1; j >= 0 ; j --){
                ptrB = myArray[j];
                if (ptrB > ptrA){
                    //swap
                    myArray[j] = ptrA;
                    myArray[j + 1] = ptrB;
                }
            }
        }
        for (int i = 0; i < size; i ++ ){
            System.out.print(myArray[i] + " ");
        }
    }
}
