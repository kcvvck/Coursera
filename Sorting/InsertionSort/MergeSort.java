
/**
 * Write a description of MergeSort here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import java.util.Scanner;

public class MergeSort {
    public static void main(String[] args){
        Scanner sc = new Scanner(System.in);
        
        System.out.println("Enter the number of elements: ");
        int size = sc.nextInt();
        
        int[] myArray = new int[size];
        // must leave a space! in your input
        System.out.println("Input your elements ");
        
        for (int i = 0; i < size; i ++ ){
            myArray[i] = sc.nextInt();
        }
        
        mergeSort(myArray, 0, size);
    }
    
    public static int[] mergeSort(int[] ar, int start, int end){
        if (start == end){
            return ar;
        }else{
            int half = end/2;
            mergeSort(ar, start, half);
            mergeSort(ar, half + 1, end);
            
        }
        return merge(ar, start, end/2, end);
    }
    
    public static int[] merge(int[] ar, int start,int middle,int end){
        
        int i = 0;
        int j = 0;
        int[] temp = new int[end];
        int count = 0;
        // while both arrays are not finished
        while(i < middle && j < end && count < end){
            if (ar[i] < ar[j]){
                temp[count] = ar[i];
                i++;
                count ++;
            }else if (ar[i] == ar[j]){
                temp[count + 1] = ar[j];
                i++;
                j++;
                count+= 2;
            }else{
                temp[count] = ar[j];
                j++;
                count ++;
            }
        }
        return temp;
    }
}
