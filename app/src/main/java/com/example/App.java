package com.example;

import com.example.application.FizzBuzzListCommand;
import com.example.application.FizzBuzzValueCommand;
import com.example.domain.model.FizzBuzzValue;
import com.example.domain.type.FizzBuzzType01;
import java.util.List;

/**
 * Main application class for FizzBuzz.
 */
public class App {
    /**
     * Main method to run the FizzBuzz application.
     * 
     * @param args command line arguments
     */
    public static void main(String[] args) {
        System.out.println("FizzBuzz Application");
        System.out.println("===================");
        
        // Create a FizzBuzzValueCommand with Type01
        FizzBuzzValueCommand valueCommand = new FizzBuzzValueCommand(new FizzBuzzType01());
        
        // Demonstrate single value generation
        System.out.println("\nSingle Value Examples:");
        System.out.println("---------------------");
        System.out.println("FizzBuzz for 1: " + valueCommand.execute(1));
        System.out.println("FizzBuzz for 3: " + valueCommand.execute(3));
        System.out.println("FizzBuzz for 5: " + valueCommand.execute(5));
        System.out.println("FizzBuzz for 15: " + valueCommand.execute(15));
        
        // Create a FizzBuzzListCommand with Type01
        FizzBuzzListCommand listCommand = new FizzBuzzListCommand(new FizzBuzzType01());
        
        // Demonstrate list generation
        System.out.println("\nList Example (1-20):");
        System.out.println("-------------------");
        List<FizzBuzzValue> fizzBuzzList = listCommand.executeList(20);
        
        for (int i = 0; i < fizzBuzzList.size(); i++) {
            System.out.println((i + 1) + ": " + fizzBuzzList.get(i).getValue());
        }
    }
}