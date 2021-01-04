// Create Chitty Chitty Bang Bang
// Extends of Plane, Car and Ship
// Each one do a specific required action like flying, driving or sailing
// Create everithing we need to create the class ChittyChittyBangBang
// that has to be able to do those actions

object Actions extends Enumeration {
    val RUNNING, STOPPED, FLYING, DRIVING, SAILING = Value
}

trait Car {
    def drive;
}

trait Plane {
    def fly;
}

trait Ship {
    def sail;
}

abstract class DefaultAction(action: Actions.Value){
    // Watch if engine has been turned on or not
    protected var poweredEngine = false
}


class ChittyChittyBangBang(var action: Actions.Value) extends DefaultAction(action) with Car with Plane with Ship {
    // Only affects in those status that requires the engine has to be turned on
    private def statusOnEngine(act: Actions.Value): Unit = {
        if(poweredEngine == true) {
            action = act
            act match {
                case Actions.DRIVING => println("You are driving!")
                case Actions.FLYING => println("You are flying!")
                case Actions.SAILING => println("You are sailing!")
            }
        } else {
            println("The engine is turned off")
            act match {
                case Actions.DRIVING => println("You cannot drive!")
                case Actions.FLYING => println("You cannot fly!")
                case Actions.SAILING => println("You cannot sail!")
            }
        }
    }

    private def changeStatus(act: Actions.Value): Unit = act match {
        case Actions.STOPPED => {
            if(poweredEngine == false) {
                println("The engine is already off")
            } else {
                println("Turning off the engine")
                poweredEngine = false;
                action = Actions.STOPPED
                println("The engine has been turned off successfully")
            }
        }
        case Actions.RUNNING => {
            if(poweredEngine == true) {
                println("The engine is already on")
            } else {
                println("Turning on the engine")
                poweredEngine = true;
                action = Actions.RUNNING
                println("The engine has been turned on successfully")
            }
        }
        case Actions.DRIVING => statusOnEngine(act)
        case Actions.FLYING => statusOnEngine(act)
        case Actions.SAILING => statusOnEngine(act)
    }
    
    def stop(): Unit = changeStatus(Actions.STOPPED)

    def run(): Unit = changeStatus(Actions.RUNNING)

    def drive(): Unit = changeStatus(Actions.DRIVING)
    
    def fly(): Unit = changeStatus(Actions.FLYING)
    
    def sail(): Unit = changeStatus(Actions.SAILING)
    
    def status(): Unit = action match {
        case Actions.STOPPED => println("Stopped!")
        case Actions.RUNNING => println("Running!")
        case Actions.DRIVING => println("Driving!")
        case Actions.FLYING => println("Flying!")
        case Actions.SAILING => println("Sailing!")
    }

}

object MainObject extends App {
    override def main(args: Array[String]): Unit = {
        val ccbb = new ChittyChittyBangBang(Actions.STOPPED)
    }
}