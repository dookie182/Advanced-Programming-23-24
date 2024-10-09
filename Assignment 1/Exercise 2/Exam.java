/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package apexercise.assignment_2;

/**
 *
 * @author Francesco Kotopulos De Angelis
 */
@XMLable
public class Exam {
    
    @XMLfield(type = "String")
    public String name;
    
    @XMLfield(type = "boolean", name = "active")
    public boolean isActive;
    
    @XMLfield(type = "int")
    private int code;
    
    public Exam(){}
    
    public Exam(String name, boolean active, int code)
    {
        this.name = name;
        this.isActive = active;
        this.code = code;
    }
}