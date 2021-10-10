
/**
 * Write a description of caesarBreaker here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import edu.duke.*;

public class caesarBreaker {
    public void main(String[] args){
        testDecrypt();
    }
    
    public int[] countLetters(String message){
        String alpha = "abcdefghijklmnopqrstuvwxz";
        int counter[] = new int[26];
        message = message.toLowerCase();
        for(int i = 0 ; i < message.length(); i++){
            int idx = alpha.indexOf(message.charAt(i));
            if(idx != -1){
                counter[idx] += 1;
            }
        }
        return counter;
    }
    
    public int maxIndex(int counter[]){
        int maxIn = 0;
        int maxVal = counter[maxIn];
        for(int i = 0 ; i < counter.length ; i++){
            if(counter[i] > maxVal){
                maxVal = counter[i];
                maxIn = i;
            }
        }
        return maxIn;
    }
    
    public String halfOfString(String message, int start){
        String s = "";
        for(int i = start ; i < message.length(); i= i + 2){
            s = s + message.charAt(i);
        }
        return s;
    }
    
    public int getKey(String s){
        int counter[] = countLetters(s);
        int maxIn = maxIndex(counter);
        int dkey = maxIn - 4;
        if (maxIn < 4) {
            dkey = 26 - (4 - maxIn);
        }
        return dkey;
    }
    
    public String decryptTwoKeys(String encrypted){
        CaesarCipher cc = new CaesarCipher();
        
        String s1 = halfOfString(encrypted, 0);
        String s2 = halfOfString(encrypted, 1);
        int key1 = getKey(s1);
        int key2 = getKey(s2);
        System.out.println(key1 + " " + key2);
        return cc.encryptTwoKeys(encrypted, 26 - key1, 26 - key2);
    }
    
    public void testDecrypt(){
        System.out.println(decryptTwoKeys("Aal uttx hm aal Qtct Fhljha pl Wbdl. Pvxvxlx!"));
    }
}
