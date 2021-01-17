// Create Chitty Chitty Bang Bang
// Extends of Plane, Car and Ship
// Each one do a specific required action like flying, driving or sailing
// Create everithing we need to create the class ChittyChittyBangBang
// that has to be able to do those actions

sealed trait Actions 

case object FLYING extends Actions
case object DRIVING extends Actions
case object SAILING extends Actions

trait Engine {
    var status: Option[Actions] = None
    def changeStatus(act: Actions): Unit = act match {
        case DRIVING => status = Option(DRIVING)
        case FLYING => status = Option(FLYING)
        case SAILING => status = Option(SAILING)
    }
}

trait Car extends Engine {
    def drive(): Unit = changeStatus(DRIVING)
}

trait Plane extends Engine {
    def fly(): Unit = changeStatus(FLYING)
}

trait Ship extends Engine{
    def sail(): Unit = changeStatus(SAILING)
}


class ChittyChittyBangBang extends Car with Plane with Ship {
    def getStatus() = status match {
        case None => println("The engine is turned off")
        case Some(DRIVING) => println("Driving!")
        case Some(FLYING) => println("Flying!")
        case Some(SAILING) => println("Sailing!")
    }
}

object MainObject extends App {
    override def main(args: Array[String]): Unit = {
        val ccbb = new ChittyChittyBangBang()
    }
}