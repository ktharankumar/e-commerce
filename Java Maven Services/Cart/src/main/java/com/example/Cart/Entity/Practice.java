package com.example.Cart.Entity;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Practice {
    public static void main(String[] args) {
        ArrayList<BigDecimal> price = new ArrayList<BigDecimal>();
        price.add(BigDecimal.valueOf(1000));
        price.add(BigDecimal.valueOf(100));
        price.add(BigDecimal.valueOf(1001));
        price.add(BigDecimal.valueOf(1999));
        price.add(BigDecimal.valueOf(10));



        BigDecimal Result = price.stream().filter(x -> x.compareTo(BigDecimal.valueOf(1000)) > 0).
                                        reduce(BigDecimal.ZERO, BigDecimal::add);

        Map<String, List<BigDecimal>> OddAndEvens=
        price.stream().collect(Collectors.groupingBy(p -> {
            if (p.remainder(BigDecimal.valueOf(2)).compareTo(BigDecimal.ZERO) == 0) {
                return "Even";
            }
            else{
                return "Odd";
            }
        }));
        System.out.println(OddAndEvens);
        System.out.print("Result: " + Result);
    }

}
