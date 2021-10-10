
/**
 * Write a description of wordFrequencies here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import edu.duke.*;
import java.util.ArrayList;

public class wordFrequencies {
    private ArrayList<String> myWords;
    // kth position in myFreqs should represent the number of times the kth word
    // in myWords[] occurs
    private ArrayList<Integer> myFreqs;
    
    public wordFrequencies(){
        //FileResource fr = new FileResource();
        //String text = fr.toString();
        this.myWords = new ArrayList<String>();
        this.myFreqs = new ArrayList<Integer>();
        
    }
    public void findUnique(){
        myWords.clear();
        myFreqs.clear();
        FileResource fr = new FileResource();
        for (String words : fr.words()){
            words = words.toLowerCase();
            if (myWords.indexOf(words) == -1){
                myWords.add(words);
                myFreqs.add(1);
            }else{
                int index = myWords.indexOf(words);
                int value = myFreqs.get(index);
                myFreqs.set(index, value+1);
            }
        }
    }
    public void tester(){
        findUnique();
        System.out.println("The size of this array is " + myWords.size());
        System.out.println("These are my words " + myWords);
        System.out.println("These are the frequencies " + myFreqs);
        System.out.println("Max index " + myFreqs.get(findIndexOfMax()) + myWords.get(findIndexOfMax()));
    }
    public int findIndexOfMax(){
        int max = 0;
        int maxIndex = 0;
        for(int i = 0 ; i < myFreqs.size(); i++){
            if (myFreqs.get(i) > max){
                max = myFreqs.get(i);
                maxIndex = i;
            }
        }
        return maxIndex;
    }
}
