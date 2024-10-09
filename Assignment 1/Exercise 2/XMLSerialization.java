/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package apexercise.assignment_2;

import java.io.IOException;
import java.util.ArrayList;

/**
 *
 * @author Francesco Kotopulos De Angelis
 */
public class XMLSerialization {
    
        public static void main(String[] args) throws IOException {

            Student s1 = new Student("Francesco", "Kotopulos", 27);
            Exam e1 = new Exam("Advanced Programming", true, 100);
            Student s2 = new Student("John", "Doe", 21);
            Taxes t1 = new Taxes(549045,true);

            ArrayList<Object> objects = new ArrayList();

            objects.add(s1);
            objects.add(e1);
            objects.add(s2);
            objects.add(t1);

            XMLSerializer.serialize(objects.toArray(),"Serialized");
        }
}
