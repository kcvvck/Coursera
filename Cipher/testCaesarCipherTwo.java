
/**
 * Write a description of testCaesarCipherTwo here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import edu.duke.*;

public class testCaesarCipherTwo {
    public String halfOfString(String message, int start){
        String s = "";
        for(int i = start ; i < message.length(); i= i + 2){
            s = s + message.charAt(i);
        }
        return s;
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
    
    public String decryptTwoKeys(String encrypted){
        String s1 = halfOfString(encrypted, 0);
        String s2 = halfOfString(encrypted, 1);
        
        int a[] = countLetters(s1);
        int b[] = countLetters(s2);
        
        int aMaxIn = maxIndex(a);
        int bMaxIn = maxIndex(b);
        
        int akey = aMaxIn - 4;
        int bkey = bMaxIn - 4;
        
        if (aMaxIn < 4) {
            akey = 26 - (4 - aMaxIn);
        }
        if (bMaxIn < 4) {
            bkey = 26 - (4 - bMaxIn);
        }
        
        CaesarCipherTwo cc = new CaesarCipherTwo(akey, bkey);
        return cc.decrypt(encrypted);
    }
    
    public void simpleTests(){
        FileResource fr = new FileResource();
        //CaesarCipherTwo cc = new CaesarCipherTwo(14,24);
        String message = fr.asString();
        //String encrypted = decryptTwoKeys(message);
        //String encrypted = cc.encrypt(message);
        //System.out.println(encrypted + "\n" + cc.decrypt(encrypted));
        String decryptMessage = this.decryptTwoKeys(message);
        System.out.println(decryptMessage);
    }
}
