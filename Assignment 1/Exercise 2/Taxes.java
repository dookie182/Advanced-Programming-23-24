/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package apexercise.assignment_2;

/**
 *
 * @author Francesco Kotopulos De Angelis
 */
public class Taxes {
    private int matricola;
    private boolean toPay;
    
    public Taxes(int matricola,boolean paid){
        this.matricola = matricola;
        this.toPay = !paid;
    }
            
    
}
