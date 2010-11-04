package blueeyes.util

case class CommandLineArguments private (parameters: Map[String, String], values: List[String]) {
  def + (that: CommandLineArguments): CommandLineArguments = new CommandLineArguments(this.parameters ++ that.parameters, this.values ++ that.values)
  
  def size = parameters.size + values.length
}
object CommandLineArguments {
  def apply(args: String*): CommandLineArguments = {
    import scala.util.matching.Regex

    var Key   = """-{1,2}(.+)""".r
    var Value = """([^\-].*)""".r

    def parse0(args: List[String]): CommandLineArguments = args match {
      case Key(opt1) :: (second @ Key(opt2)) :: rest => 
        new CommandLineArguments(Map(opt1 -> ""), Nil) + parse0(second :: rest)

      case Key(opt1) :: Value(val1) :: rest =>
        new CommandLineArguments(Map(opt1 -> val1), Nil) + parse0(rest)

      case Value(val1) :: rest =>
        new CommandLineArguments(Map(), val1 :: Nil) + parse0(rest)
        
      case Key(opt1) :: Nil => 
        new CommandLineArguments(Map(opt1 -> ""), Nil)
      
      case Nil =>
        new CommandLineArguments(Map(), Nil)
    }
    
    parse0(args.toList)
  }
}