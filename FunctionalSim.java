/*
Andrew Gerlach
Finding vowels and consonants in a given String.
*/
import java.io.*;

public class FunctionalSim {
    /*Method v_Count
    *Takes in a String and returns the number of vowels in the String
    *for recursive calls not using 'length() -1'
    *because it doesn't include the last letter in the argument
    */
	static int v_Count(String word){
		//One letter case
		if ((word.length() == 1) && ((word.charAt(0) =='a')||
			(word.charAt(0)=='e')||(word.charAt(0) == 'i')||(word.charAt(0) == 'o')||(word.charAt(0) == 'u'))){
			return 1;
	    }
	    else if((word.length() == 1) && !((word.charAt(0) =='a')||
			(word.charAt(0)=='e')||(word.charAt(0) == 'i')||(word.charAt(0) == 'o')||(word.charAt(0) == 'u'))){
	    	return 0;
	    }
	    //more than one letter case
		if((word.charAt(0) =='a')||(word.charAt(0)=='e')||(word.charAt(0) == 'i')||(word.charAt(0) == 'o')||(word.charAt(0) == 'u')){
			//found a vowel
			//call the function again using the same string with the first letter removed
			//add 1
			return 1 + v_Count(word.substring(1,word.length()));
		}
		else{
			//no vowel
			//call the function again but don't add
			return v_Count(word.substring(1,word.length()));
		}
	}
    public static void main(String args[]) throws IOException{
    	BufferedReader keyboard = new BufferedReader(new InputStreamReader(System.in));
    	System.out.println("Enter a word: ");
    	String input = keyboard.readLine();
    	System.out.println("In "+input+", there are "+(v_Count(input))+" vowels and "+(input.length()-(v_Count(input)))+" consonants.");
    	}
	}