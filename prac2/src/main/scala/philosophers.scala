import ox.CSO._, ox.cso.Components

object Philosophers {  

  val N = 5; // Number of philosophers
  val random = new scala.util.Random

  val report = ManyOne[String];
  
  def Eat = sleep(500);
  def Think = sleep(random.nextInt(800));
  def Pause = sleep(500);

  // is_right indicates whether the philosopher is right handed or not, and has_butler indicated whether there is a butler present
  def Phil(me: Int, left: ![String], right: ![String], sit: ![Int], leave: ![Int], permission: ?[Int], is_right: Boolean, has_butler: Boolean) = proc {
    repeat {
      Think;

      // If there is a butler, then the philosopher makes a request for seating before
      // waiting for permission from the butler.
      if (has_butler) {
        sit!(me);
        val i = permission?;
      }
      
      report!(me+" sits"); Pause;

      // If the Philopsopher is left handed we pick up the left fork first, otherwise
      // we pick up the right fork first.
      if (!is_right) {
        left !("pick");
        report!(me+" picks up left fork"); Pause;
        right!("pick");
        report!(me+" picks up right fork"); Pause;
      } else {
        right!("pick");
        report!(me+" picks up right fork"); Pause;
        left !("pick");
        report!(me+" picks up left fork"); Pause;
      }
      report!(me+" eats"); Eat;
      left!("drop"); Pause; right!("drop"); Pause;

      // If there is a butler, then the philosopher notifies the butler that he is 
      // leaving
      if (has_butler) {
        leave!(me)
      }
      
      report!(me+" leaves");
    }
  }

  def Fork(me: Int, left: ?[String], right: ?[String]) = proc("Fork"+me) {
    serve (
      left --> {
        val x = left?; assert(x=="pick");
        val y = left?; assert(y=="drop");
      }
      | right --> {
        val x = right?; assert(x=="pick");
        val y = right?; assert(y=="drop");       
      }
    )
  }

  val sit, leave = ManyOne[Int];
  val philToLeftFork, philToRightFork = OneOne[String](N)
  val philPermission = OneOne[Int](N)

  def AllPhils(is_right: Boolean, has_butler: Boolean) = || (
    // If we are using the right handed method, the last philosopher is initialised as right handed
    for (i <- 0 until N) yield 
      Phil(i, philToRightFork(i),philToLeftFork(i),sit,leave,philPermission(i),is_right && i==N-1, has_butler)
  )

  def AllForks = || (
    for (i <- 0 until N) yield 
      Fork(i, philToRightFork((i+1)%N),philToLeftFork(i))
  )

  def Butler(sit: ?[Int], leave: ?[Int], has_butler: Boolean) = proc {
    if (has_butler) {
      // Keep track of the total number of people sitting and those who are seated
      var sitting = 0;
      var seated = Array.fill(N)(false);
      serve (
        // Request for sitting, guard ensures that no more than 4 philosophers are seated
        (sitting < N-1 &&& sit) --> {
          // Asserts that the philosopher is not already seated
          val i = sit?; assert(!seated(i));
          seated(i) = true;
          sitting+=1;
          // Notifies the philosopher that he has been seated
          philPermission(i)!sitting;
        }
        // Request to leave
        | (sitting >= 0 &&& leave) --> {
          // Asserts that the philosopher is seated
          val i = leave?; assert(seated(i));
          seated(i) = false;
          sitting-=1;
        }
      )
    }
  }

  // argument for whether we avoid deadlock via the butler or right handed philosopher method
  val arg = "right" // "butler" or "right"

  def System = AllPhils(arg=="right", arg=="butler") || AllForks || Components.console(report) || Butler(sit, leave, arg=="butler")

  def main(args: Array[String]) = System()
}

// RIGHT HANDED PHILOSOPHER OUTPUT
// Note that 4 picks up his right fork first
// 3 sits
// 2 sits
// 0 sits
// 4 sits
// 1 sits
// 3 picks up left fork
// 2 picks up left fork
// 0 picks up left fork
// 1 picks up left fork
// 3 picks up right fork
// 3 eats
// 2 picks up right fork
// 2 eats
// 3 leaves
// 1 picks up right fork
// 1 eats
// 3 sits
// 2 leaves
// 0 picks up right fork
// 3 picks up left fork
// 2 sits
// 0 eats
// 3 picks up right fork
// 2 picks up left fork
// 1 leaves
// 4 picks up right fork
// 3 eats
// 1 sits
// 2 picks up right fork
// 1 picks up left fork
// 0 leaves
// 2 eats
// 4 picks up left fork

// BUTLER
// Note that no more than 4 philosophers are ever seated
// 2 sits
// 3 sits
// 1 sits
// 4 sits
// 2 picks up left fork
// 3 picks up left fork
// 1 picks up left fork
// 4 picks up left fork
// 4 picks up right fork
// 4 eats
// 3 picks up right fork
// 3 eats
// 2 picks up right fork
// 4 leaves
// 0 sits
// 2 eats
// 0 picks up left fork
// 3 leaves
// 4 sits
