import ox.CSO._, ox.cso.Components

// Buffering is required on any channel outputting to a merge process' input. This is required
// due to the fact that merge may not nesscecarly immediately read from a channel, for example
// if the first number written to in1 is 200, and in2 keeps reading numbers less than 200, a
// backlog of numbers will start to form on in1 as we will not be accepting input from the 
// channel.
//
// 1000th Hamming No: 51200000
//
// Output without Buffers:
//   1
//   2
//   3
//   4
//   5
//   6
//   [Deadlocks here]

object Hamming {  

  def prefix(i: Int, out: ![Int]) = proc("prefix") { out!i }

  // Basic Tee function which accepts a value on its in
  // channel and broadcats the value out on channels 
  // out1 and out2.
  def tee[T](in: ?[T], out1: ![T], out2: ![T]) = proc("tee") {
    repeat {
      val v = in?;
      (proc{out1!v} || proc(out2!v))()
    };
    in.close; out1.close; out2.close;
  }

  // Merge reads the two values from in1 and in2. If in1 and in2 are
  // equal, it outputs their value on channel out. If in1 is less
  // than in2, in1 is output on channel out, and the next value is
  // read from channel in1 for compaison with the last read value on
  // in2. The opposite occurs when in2 is less than in1.
  def merge(in1: ?[Int], in2: ?[Int], out: ![Int]) = proc("merge") {
    var x = in1?;
    var y = in2?;
    repeat {
      if (x == y) {
        out!x;
        x = in1?;
        y = in2?;
      } else if (x < y) {
        out!x;
        x = in1?;
      } else if (x > y) {
        out!y;
        y = in2?;
      }
    };
    in1.close; in2.close; out.close;
  }

  // Outputs an input channel value multiplied by a factor 
  // i
  def x(i: Int, in: ?[Int], out: ![Int]) = proc("factor") { 
    repeat { 
      out!(i * (in?)) 
    } 
    in.close; out.close;
  }

  // Counts the number of values received on an input channel
  // and terminates when a limit, n, is reached. All input is
  // forwarded along an output channel.
  def count(n: Int, in: ?[Int], out: ![Int]) = proc("count") { 
    var i = 0
    while (i < n) {
      out!(in?);
      i += 1;
    }
    in.close; out.close;
  }

  val in, out, c, xi2, xi3, xi5, t1, t2 = OneOne[Int];
  val xo2, xo3, xo5, m = Buf[Int](1000);

  def System = 
    prefix(1,in) ||
    tee(in, t1, c) ||
    tee(t1, xi2, t2) ||
    x(2,xi2, xo2) ||
    tee(t2,xi3,xi5) ||
    x(3,xi3,xo3) ||
    x(5,xi5,xo5) ||
    merge(xo2,xo3,m) ||
    merge(m,xo5,in) ||
    count(1000,c,out) ||
    Components.console(out)

  def main(args: Array[String]) = System()

}