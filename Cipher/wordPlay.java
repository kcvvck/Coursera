
/**
 * Write a description of wordPlay here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */

import java.util.Locale;

public class wordPlay {
    public static void main(String[] args){
        System.out.println(emphasize("Mary Bella Abracadabra", 'a'));
    }
    
    public static boolean isVowel(char ch){
        char[] vowelList = {'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'};
        if (new String(vowelList).indexOf(ch) != -1){
            return true;
        }else{
            return false;
        }
    }
    
    public static String replaceVowel(String phrase, char ch){
        char[] out = new char[phrase.length()];
        
        for (int i = 0; i < phrase.length(); i ++){
            if (isVowel(phrase.charAt(i))){
                out[i] = ch;
            }else{
                out[i] = phrase.charAt(i);
            }
        }
        String replacedVowel = new String(out);
        return replacedVowel;
    }
    
    public static boolean isChar(char IHave, char IWant){
        IHave = Character.toLowerCase(IHave);
        IWant = Character.toLowerCase(IWant);
        if (Character.compare(IHave, IWant) == 0){
            return true;
        }else{
            return false;
        }
    }
    
    public static String emphasize(String phrase, char ch){
        char[] out = new char[phrase.length()];
        for (int i = 0; i < phrase.length(); i++){
            if (isChar(phrase.charAt(i), ch)){
                if ((i+1) % 2 != 0){
                    out[i] = '*';
                }else{
                    out[i] = '+';
                }
            }else{
                out[i] = phrase.charAt(i);
            }
        }
        return  new String(out);
    }
}
