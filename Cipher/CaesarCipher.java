
/**
 * Write a description of CaesarCipher here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
import edu.duke.*;

public class CaesarCipher {
    public static void main(String[] args){
        //testCaesar();
        //testencryptTwoKeys();
        String a = encryptTwoKeys("Hfs cpwewloj loks cd Hoto kyg Cyy.", 26-14, 26-24);
        System.out.println(a);
    }
    public static String encrypt(String input, int key){
        StringBuilder encrypted = new StringBuilder(input);
        String alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String downAlpha = alpha.toLowerCase();
        String shiftedAlpha = alpha.substring(key) + alpha.substring(0, key);
        
        //System.out.println(shiftedAlpha);
        int idx = 0;
        for(int i = 0; i < input.length(); i++){
            char currChar = encrypted.charAt(i);
            boolean isLower = Character.isLowerCase(currChar);
            if (isLower){
                idx = downAlpha.indexOf(currChar);
            }else{
                idx = alpha.indexOf(currChar);
            }
            if (idx != -1){
                char finalCurr = shiftedAlpha.charAt(idx);
                //System.out.println(finalCurr);
                if (isLower){
                    finalCurr = Character.toLowerCase(finalCurr);
                }
                encrypted.setCharAt(i, finalCurr);
            }
        }
        return encrypted.toString();
    }
    public static void testCaesar(){
        //FileResource fr = new FileResource();
        //message = fr.asString();
        int key = 15;
        String encrypted = encrypt("Can you imagine life WITHOUT the internet AND computers in your pocket?", key);
        System.out.println("key is " + key + "\n" + encrypted);
    }
    
    public static char encryptChar(char ch, int i, StringBuilder encrypted, 
                                     String largeAlpha, String smallAlpha, 
                                     String shiftedAlpha){
        
        int idx = 0;
        boolean isLower = Character.isLowerCase(ch);
        char finalChar = ' ';
        if (isLower){
            idx = smallAlpha.indexOf(ch);
        }else{
            idx = largeAlpha.indexOf(ch);
        }
        if (idx != -1){
            finalChar = shiftedAlpha.charAt(idx);
                //System.out.println(finalCurr);
            if (isLower){
                finalChar = Character.toLowerCase(finalChar);
            }
            
        }
        
        return finalChar;
    }
    
    public static String encryptTwoKeys(String input, int key1, int key2){
        StringBuilder encrypted = new StringBuilder(input);
        String alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String downAlpha = alpha.toLowerCase();
        String shiftedAlpha1 = alpha.substring(key1) + alpha.substring(0, key1);
        String shiftedAlpha2 = alpha.substring(key2) + alpha.substring(0, key2);
        char finalChar = ' ';
        for (int i = 0; i < input.length(); i++){
            if ((i+1)%2 !=0){
                finalChar = encryptChar(input.charAt(i), i, encrypted, alpha, downAlpha, shiftedAlpha1);
            }
            else{
                finalChar = encryptChar(input.charAt(i), i, encrypted, alpha, downAlpha, shiftedAlpha2);
            }
            encrypted.setCharAt(i, finalChar);
        }
        return encrypted.toString();
    }
    public static void testencryptTwoKeys(){
        //FileResource fr = new FileResource();
        //message = fr.asString();
        int key1 = 21;
        int key2 = 8;
        String encrypted = encryptTwoKeys("Can you imagine life WITHOUT the internet AND computers in your pocket?", key1, key2);
        System.out.println("key1 is " + key1 + " key2 is " + key2 + "\n" + encrypted);
    }
}

