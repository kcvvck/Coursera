
/**
 * Write a description of testCaesarCipher2 here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import edu.duke.*;

public class testCaesarCipher2 {
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
    
    public String breakCipher(String s){
        int counter[] = countLetters(s);
        int maxIn = maxIndex(counter);
        int dkey = maxIn - 4;
        if (maxIn < 4) {
            dkey = 26 - (4 - maxIn);
        }
        CaesarCipher2 cc = new CaesarCipher2(dkey);
        String decrypted = cc.decrypt(s);
        return decrypted;
    }
    
    
    public void simpleTests(){
        FileResource fr = new FileResource();
        CaesarCipher2 cc = new CaesarCipher2(18);
        String message = fr.asString();
        String encrypted = cc.encrypt(message);
        String decrypted = this.breakCipher(message);
        System.out.println(encrypted + "\n" + decrypted);
    }
}
