
/**
 * Write a description of CharactersInPlay here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import java.util.ArrayList;
import edu.duke.*;

public class CharactersInPlay {
    private ArrayList<String> charName;
    private ArrayList<Integer> counter;
    
    public CharactersInPlay(){
        this.charName = new ArrayList<String>();
        this.counter = new ArrayList<Integer>();
    }
    public void update(String person){
        int index = charName.indexOf(person);
        if (index == -1){
            charName.add(person);
            counter.add(1);
        }else{
            int value = counter.get(index);
            counter.set(index, value + 1);
        }
    }
    public void findAllCharacters(){
        charName.clear();
        counter.clear();
        FileResource fr = new FileResource();
        for(String line : fr.lines()){
            if (line.contains(".")){
                int idx = line.indexOf(".");
                String possibleName = line.substring(0, idx);
                update(possibleName);
            }
        }
    }
    public void tester(){
        findAllCharacters();
        int charQuota = 3;
        for (int i = 0 ; i < charName.size() ; i ++){
            if (counter.get(i) <= charQuota){
                System.out.println(charName.get(i) + " " + counter.get(i));
            }
        }
        charactersWithNumParts(1, 5);
    }
    public void charactersWithNumParts(int num1, int num2){
        for (int i = 0 ; i < charName.size() ; i ++){
            if (counter.get(i) < num2 && counter.get(i) > num1){
                System.out.println(charName.get(i) + " " + counter.get(i));
            }
        }
    }
}
