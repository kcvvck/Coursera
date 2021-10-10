
/**
 * Write a description of wordLength here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import edu.duke.*;

public class wordLength {
    public static void main(String[] args){
        testcountWordLengths();
    }
    
    public static int[] countWordLengths(FileResource resource, int counts[]){
        //String a[] = new String[1];
        for(String word : resource.words()){
            int count = word.length();
            for (int i=0; i<word.length();i++){
                char currChar = word.charAt(i);
                if ((i==0) || (i==word.length()-1)){
                    if (!Character.isLetter(currChar)) 
                    count--;
                }
            }    
            counts[count]++;   
            System.out.println(" Words of length "+ count +" "+ word);
            
        }
        //for(int k=0; k< counts.length; k++){
            //if (counts[k]> 0){
                //System.out.println(counts[k] + " words of length " + k);
            //}
        //}
        return counts;
    }
    
    public static int indexOfMax(int values[]){
        int maxIn = 0;
        int maxVal = values[maxIn];
        for(int k=0; k< values.length; k++){
            if (values[k]> maxVal){
                maxVal = values[k];
                maxIn = k;
            }
        }
        return maxIn;
    }
    public static void testcountWordLengths(){
        FileResource fr = new FileResource();
        int counts[] = new int[31];
        int maxIn = indexOfMax(countWordLengths(fr, counts));
        System.out.println(maxIn);
    }
}
