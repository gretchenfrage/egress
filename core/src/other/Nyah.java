package other;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Scanner;
import java.util.stream.Stream;

/**
 * Created by Phoenix on 8/4/2017.
 */
public class Nyah {

    public static void main(String[] args) {
        System.out.println(binarize(input));
    }

    public static String binarize(String input) {
        input = input.chars()
                .filter(i -> Character.isDigit(i) || Character.isWhitespace(i))
                .collect(StringBuilder::new, (builder, i) -> builder.append((char) i), StringBuilder::append)
                .toString();
        System.out.println("input = " + input);
        Scanner scanner = new Scanner(input);
        StringBuilder builder = new StringBuilder();
        while (scanner.hasNext()) {
            builder.append(new BigInteger(scanner.next()).toString(2));
            builder.append('\n');
        }
        return builder.toString();
    }

    public static String input = "[21865874, 23901338, 26659236, 28629292]  \n" +
            "[52876132, 56939116]  \n" +
            "[]  \n" +
            "[]  \n" +
            "[347491629, 378949029]  \n" +
            "[846015643, 911027603]  \n" +
            "[]  \n" +
            "[]  \n" +
            "[]  \n" +
            "[11111605651, 11195229586, 12120274075, 12237517978, 13532318117, 13649496484, 14574475565, 14649963099, 14658230572]  \n" +
            "[]  \n" +
            "[54573813036]  \n" +
            "[96962458203, 97868108397, 97868428443, 116596593261, 117200574765]  \n" +
            "[177917450540, 179094236314, 194021607844, 218392474202, 234531173074]  \n" +
            "[358250500716, 391599543140, 433161286810, 466445247890]  \n" +
            "[711140684499, 716375172499, 866066289509, 873179826597]  \n" +
            "[]  \n" +
            "[]  \n" +
            "[]  \n" +
            "[14924363969115, 15001673381019]  \n" +
            "[24822389330541, 25054317566253]  \n" +
            "[45513003838618, 45848011290202, 55428242623788, 55883509160556]  \n" +
            "[]  \n" +
            "[]  \n" +
            "[]  \n" +
            "[]  \n" +
            "[1457499757291219, 1589425014912603, 1788274709056932, 1789071157054885, 1920199965691180, 1921279344659757]  \n" +
            "[2912840701388498, 2914971007169114, 3177257143711322, 3547415983639396, 3548446777856620, 3820611339180652]  \n" +
            "[5868559940660626, 5869576237297043, 6413884358995098, 6415966881262747, 7096914530224997, 7642238946531949]  \n" +
            "[]  \n" +
            "[23478098184709843, 28625068885429093]";

}
