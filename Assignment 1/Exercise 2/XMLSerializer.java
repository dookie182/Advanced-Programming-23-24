/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 */

package apexercise.assignment_2;

import java.util.ArrayList;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;

/**
 *
 * @author Francesco Kotopulos De Angelis
 */
public class XMLSerializer {
    
    public XMLSerializer(){}

    public static void serialize(Object[] arr, String fileName) throws IOException {
        try (FileWriter writer = new FileWriter(fileName + ".xml")) {
            writer.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
            
            for (Object obj : arr) {
                Class objClass = obj.getClass();

                // Retreiving informations about Class if available
                if (objClass.isAnnotationPresent(XMLable.class)) {
                    writer.write("<" + objClass.getSimpleName() + ">\n");

                    // Iteration on object serializable fields
                    for (Field field : objClass.getDeclaredFields()) {
                        if (field.isAnnotationPresent(XMLfield.class)) {
                            field.setAccessible(true);  
                            
                            XMLfield xmlField = field.getAnnotation(XMLfield.class);
                            String fieldName = xmlField.name().isEmpty() ? field.getName() : xmlField.name();
                            String fieldType = xmlField.type();
                            
                            writer.write("  <" + fieldName + " type=\"" + fieldType + "\">");
                            writer.write(String.valueOf(field.get(obj))); 
                            writer.write("</" + fieldName + ">\n");
                        }
                    }

                    writer.write("</" + objClass.getSimpleName() + ">\n");
                } else {
                    // Class is not serializable
                    writer.write("<notXMLable />\n");
                }
            }
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }
}

    

    


