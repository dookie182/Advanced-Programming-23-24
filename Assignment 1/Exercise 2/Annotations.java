/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package apexercise.assignment_2;

/**
 *
 * @author Francesco Kotopulos De Angelis
 */
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@interface XMLfield{
    String type ();
    String name () default "";
}

@Retention(RetentionPolicy.RUNTIME)
@interface XMLable{ 
}