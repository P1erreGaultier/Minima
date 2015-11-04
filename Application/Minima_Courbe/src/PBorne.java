import javax.swing.*; 
import java.awt.event.*; 
import java.awt.*; 
import javax.swing.JDialog; 

public class PBorne extends JDialog 
{ 

/**
	 * 
	 */
private static final long serialVersionUID = 1L;
private JTextField TextA; 
public double PointA; 
private JTextField TextB; 
public double PointB; 


public PBorne(JFrame parent,boolean modal,double A,double B) 
{ 
super(parent, modal); 
PointA = A; 
PointB = B; 
Container contentPane = this.getContentPane(); 
JPanel panel1 = new JPanel(); 
panel1.setLayout(new FlowLayout(FlowLayout.CENTER)); 
contentPane.add(panel1, BorderLayout.SOUTH); 
JPanel panel3 = new JPanel(); 
panel3.setLayout(new FlowLayout(FlowLayout.CENTER)); 
contentPane.add(panel3, BorderLayout.CENTER); 
TextA = new JTextField(""+PointA,15); 
TextB = new JTextField(""+PointB,15); 
panel3.add(TextA); 
panel3.add(TextB); 
JButton okButton = new JButton("OK"); 
panel1.add(okButton); 
JButton cancelButton = new JButton("Cancel"); 
panel1.add(cancelButton); 

setTitle("Gestion des valeurs aux bornes"); 
pack(); 


cancelButton.addActionListener(new ActionListener(){ 
public void actionPerformed(ActionEvent event) 
{ 
dispose(); 
setVisible(false); 
} 
}); 

okButton.addActionListener(new ActionListener(){ 
public void actionPerformed(ActionEvent event) 
{ 
PointA = Double.parseDouble(TextA.getText()); 
PointB = Double.parseDouble(TextB.getText()); 
if( PointA >= PointB ) 
{ 
PointA = -10.0; 
PointB = 10.0; 
TextA.setText(""+PointA); 
TextB.setText(""+PointB); 
return; 
} 
dispose(); 
setVisible(false); 
} 
}); 

}
}