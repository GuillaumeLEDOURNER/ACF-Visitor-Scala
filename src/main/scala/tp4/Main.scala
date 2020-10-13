package tp4

object Main {
  def main(args : Array[String]) : Unit = {
		val exp=BinExpression(Plus,VariableRef("y"),BinExpression(Minus,IntegerValue(1),IntegerValue(2)))
		val prog=Seq(Assignement("x",IntegerValue(0)),
			Seq(Assignement("y",IntegerValue(1)),
				Seq(Read("z"),
					Seq(While(BinExpression(Inf,VariableRef("x"),VariableRef("z")),
						Seq(Assignement("x",BinExpression(Plus,VariableRef("x"),IntegerValue(1))),
							Seq(Assignement("y",BinExpression(Times,VariableRef("y"),VariableRef("x"))),
								Print(VariableRef("x"))))),
						Print(VariableRef("y"))))))

		println(PrettyPrinter.stringOf(exp))
		println(PrettyPrinter.stringOf(prog))
		println("\n\n")
		println(Interpret.eval(prog,List(5)))
  }
}

object PrettyPrinter{
	def stringOf(e:Expression):String={
		e match {
			case IntegerValue(i) => i.toString
			case VariableRef(v) => v
			case BinExpression(op, e1, e2) => op match {
				case Equal => stringOf(e1) + "==" + stringOf(e2)
				case Inf => stringOf(e1) + "<" + stringOf(e2)
				case Infeq => stringOf(e1) + ">=" + stringOf(e2)
				case Minus => stringOf(e1) + "-" + stringOf(e2)
				case Plus => stringOf(e1) + "+" + stringOf(e2)
				case Times => stringOf(e1) + "*" + stringOf(e2)
			}
		}
	}

	def stringOf(p:Statement):String={
		p match {
			case Assignement(v, e) => v + ":= " + "(" + stringOf(e) + ")"
			case If(c, s1, s2) => "if (" + stringOf(c) + ") then {\n" + stringOf(s1) + "} \nelse \n{" + stringOf(s2)+ "}"
			case Print(e) => "print(" + stringOf(e) + ")"
			case Read(s) => "read(" + s + ")"
			case Seq(s1, s2) => stringOf(s1) + "\n" + stringOf(s2)
			case While(c, s) => "while("+ stringOf(c) + ") do \n{ \n" + stringOf(s) + "\n}" 
		}
	}
}

object Interpret{
	var variables = Map[String,Int]() 
	var input = List[Int]()
	var output = List[Int]()

	def eval(p:Statement,inList:List[Int]):List[Int]={
		input = inList
		evalAux(p)
		println("output = " )
		output
	}
	
	def evalAux(p:Statement):List[Int]={
		p match {
			case Seq(s1, s2) => evalAux(s1) ; evalAux(s2)
			case Print(e) => output = output ::: List(eval(e)) ; Nil
			case Read(s) => variables += ( s -> input.head) ; Nil
			case If(c, s1, s2) => if (eval(c)==1) { evalAux(s1) } else { evalAux(s2) }
			case Assignement(v, e) => variables.get(v) match {
				case None => variables += (v -> eval(e)); Nil
				case Some(value) => variables += ( v -> (eval(e))) ; Nil 
			}
			case While(c, s) => while (eval(c)==1){	evalAux(s)} ; Nil			
		}
	}

	def eval(e:Expression): Int = {
		e match {
			case IntegerValue(i) => i
			case VariableRef(s) => variables.get(s) match {
				case None => -1
				case Some(value) => value
			}
			case BinExpression(op, e1, e2) => op match {
				case Minus => eval(e1) - eval(e2)
				case Plus => eval(e1) + eval(e2)
				case Times => eval(e1) * eval(e2)
				case Inf => if(eval(e1) < eval(e2)) { 1 } else { 0 }
				case Infeq => if(eval(e1) <= eval(e2)) { 1 } else { 0 }
				case Equal => if (eval(e1) == eval(e2)) { 1 } else { 0 }
 			}
		}
	}
}