package swingame.samples;

import java.util.Scanner;
import swingame.emulator.EmulatedCore;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class Test 
{
    public static void main(String[] args)
    {
        EmulatedCore.openGraphicsWindow("Hello World", 800, 600);
        
        Scanner s = new Scanner(System.in);
        s.next();
    }
}
