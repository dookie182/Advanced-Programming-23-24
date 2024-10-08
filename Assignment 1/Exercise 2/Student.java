/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package apexercise.assignment_2;
/**
 *
 * @author Koto
 */
@XMLable
public class Student {
    
    @XMLfield(type = "String")
    public String firstName;
    
    @XMLfield(type = "String", name = "surname")
    public String lastName;
    
    @XMLfield(type = "int")
    private int age;
    
    public Student(){}
    
    public Student(String fn, String ln, int age)
    {
        this.firstName = fn;
        this.lastName = ln;
        this.age = age;
    }
}
