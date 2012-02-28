import ox.CSO._, ox.cso.Components
import collection.mutable.HashMap

object GameOfLife {  
  type Point = (Int, Int)

  var N = 5;
  var P = 9;
  var a = new Array[Array[Boolean]](N,N);

  val patterns = HashMap(
    "blinker" ->  """0 0 0 0 0
                     0 0 1 0 0
                     0 0 1 0 0
                     0 0 1 0 0
                     0 0 0 0 0""",

    "toad" ->   """0 0 0 0 0 0
                   0 0 0 0 0 0
                   0 0 1 1 1 0
                   0 1 1 1 0 0
                   0 0 0 0 0 0
                   0 0 0 0 0 0""",

    "pulsar" -> """0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                   0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0
                   0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0
                   0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0
                   0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0
                   0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0
                   0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0
                   0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0
                   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0""",

    "spaceship" ->  """0 0 0 0 0 0 0 0 0
                       0 0 1 0 0 1 0 0 0
                       0 0 0 0 0 0 1 0 0
                       0 0 1 0 0 0 1 0 0
                       0 0 0 1 1 1 1 0 0 
                       0 0 0 0 0 0 0 0 0 
                       0 0 0 0 0 0 0 0 0 
                       0 0 0 0 0 0 0 0 0 
                       0 0 0 0 0 0 0 0 0""",

    "glider" ->   """0 0 0 0 0 0
                     0 0 0 1 0 0
                     0 0 0 0 1 0
                     0 0 1 1 1 0
                     0 0 0 0 0 0
                     0 0 0 0 0 0"""
  );

  def neighbours(point: Point):List[Boolean] = {
    var (x, y) = point;
    // t | n n n  
    // y | n p n  
    // b | n n n 
    //     - - -
    //     l x r   
    var l = if (x==0) N-1 else x-1
    var r = if (x==N-1) 0 else x+1
    var t = if (y==0) N-1 else y-1
    var b = if (y==N-1) 0 else y+1

    var ns = List[Boolean]();
    ns = ns :+ a(l)(t) :+ a(x)(t) :+ a(r)(t);
    ns = ns :+ a(l)(y)            :+ a(r)(y);
    ns = ns :+ a(l)(b) :+ a(x)(b) :+ a(r)(b);
    return ns;
  }

  def aliveNeighbours(point: Point):Int = {
    var ns = neighbours(point);
    return ns.filter(n => n).length;
  }
 
  def parition():IndexedSeq[(Point, Point)] = {
    var p = scala.math.sqrt(P).round.toInt
    var step = N/p;
    var rem = N%p;
    return (0 until P).map( i =>  {
      val px = i%p;
      val py = i/p;
      val pxr = if (px == p-1) rem else 0;
      val pyr = if (py == p-1) rem else 0;
      ( ((px * step),(py * step)), ((px+1)*step+pxr, (py+1)*step+pyr) )
    });
  }

  def loadPattern(name: String) = {
    var pattern = patterns(name).replaceAll("(\n)*( )*", "").toArray.map(s => s.toInt-'0'.toInt == 1);
    N = scala.math.sqrt(pattern.length).toInt;
    a = pattern.grouped(N).toArray.transpose;
  }

  val toWorkers     = OneMany[(Point, Point)];
  val toCollector   = ManyOne[List[(Point, Boolean)]];

  def Distributor = proc {
    loadPattern("blinker");
    val display = new Display(N,a);
    var paritions = parition();
    repeat {
      var t0 = System.currentTimeMillis;
      
      // Send array paritions to individual workers
      for (partition <- paritions) {
        toWorkers!partition;
      }
      
      // Collect updates from the workers
      var updates = List[(Point, Boolean)]();
      for (i <- 0 until P) {
        var update = toCollector?;
        updates = updates ++ update;
      }

      var tf = System.currentTimeMillis;
      println(tf - t0);
      
      // Apply updates to the array
      for (((x,y), alive) <- updates) {
        a(x)(y) = alive;
      }
      
      // Pause display then update
      sleep(250);
      display.draw;
    }
  }

  def Worker = proc {
    repeat {
      var ((x1,y1),(x2,y2)) = toWorkers?;
      var updates = List[(Point, Boolean)]();
      for (x <- x1 until x2) {
        for (y <- y1 until y2) {
          var n = aliveNeighbours((x,y));
          val alive = n match {
            case i if i < 2   => false;
            case i if i > 3   => false;
            case i if i == 3  => true;
            case i            => a(x)(y);
          }
          updates ::= ((x,y), alive);
        }
      }
      toCollector!updates;
    }
  }

  def Workers = || (for (i <- 0 until P) yield Worker);

  def main(args: Array[String]) = (Workers || Distributor)()
}