import scala.swing._
import java.awt.image.BufferedImage
import java.net.URL
import javax.imageio.ImageIO



class ImagePanel( rows: Int, cols: Int ) extends GridPanel( rows, cols ) {
  
   private var _imagePath = ""                                                 
   private var buf = Option.empty[ BufferedImage ]

   def imagePath = _imagePath
   
   def imagePath_=( value: String ) {
      _imagePath = value
      buf.foreach( _.flush ); buf = None
      buf = Some( ImageIO.read( new URL( value )))
      repaint()
   }

   override def paintComponent( g: Graphics2D ) {
      super.paintComponent( g )
      buf.foreach( g.drawImage( _, 0, 0, null ))
   }
}
