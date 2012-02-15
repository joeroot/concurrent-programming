import java.awt._;

class Display(N:Int, a:Array[Array[Boolean]]) extends Frame{
  // Define some constants
  private val blockSize = 10;
  private val padding = 1;
  private val gridSize = blockSize+2*padding

  // Set up the display
  private val pane = new ScrollPane(); 
  pane.setSize(N*gridSize, N*gridSize);
  private val board = new Board();
  pane.add(board);
  this.add(pane, "Center");
  this.pack();
  this.show();
  
  // Fill in all the squares
  def draw = {
    for(i <- 0 until N; j <- 0 until N)
      if(a(i)(j)) board.drawAt(i,j); else board.blankAt(i,j);
    repaint
  }

  override def paint(g:Graphics) = draw;

  class Board extends Component{
    // Define colours
    val backgroundColor = Color.white;
    val blockColor = Color.green;

    // Paint the square at (x,y) in colour c
    private def paintAt(x:Int, y:Int, c:Color) = {    
      val g = getGraphics();
      g.setColor(c);
      g.fillRect(x*gridSize+padding, y*gridSize+padding, blockSize, blockSize);
    }

    // Draw a piece
    def drawAt(x:Int, y:Int) = paintAt(x,y,blockColor);

    // Draw a blank square
    def blankAt(x:Int, y:Int) = paintAt(x,y,backgroundColor);
  }

}

