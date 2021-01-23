import scala.util.chaining.scalaUtilChainingOps
import scala.language.postfixOps
import javax.swing.{JFrame, JPanel, JLabel, Timer, AbstractAction, WindowConstants}
import java.awt.event.{KeyEvent, KeyListener, ActionEvent}
import java.awt.{Font, Graphics, Color, Dimension}

sealed trait Command
sealed trait Direction { def reversed: Direction }
case object NewGame extends Command
case object Quit extends Command
case object Up extends Direction with Command { def reversed = Down }
case object Right extends Direction with Command { def reversed = Left }
case object Left extends Direction with Command { def reversed = Right }
case object Down extends Direction with Command { def reversed = Up }

object Command {
  def fromKeyChar: PartialFunction[Char, Command] = {
    case 'n' => NewGame
    case 'q' => Quit
    case 'h' => Left
    case 'j' => Down
    case 'k' => Up
    case 'l' => Right
  }
}

sealed trait State
case object OnGoing extends State
case object Over extends State

case class Pos(x: Int, y: Int) {
  def moved(dir: Direction): Pos =
    dir match {
      case Up => Pos(x, y - 1)
      case Right => Pos(x + 1, y)
      case Left => Pos(x - 1, y)
      case Down => Pos(x, y + 1)
    }
}

case class World(snake: List[Pos], snakeDir: Direction, applePos: Pos) {
  def emptyCells: Set[Pos] =
    World.floors -- snake

  def newApplePos: Pos =
    emptyCells.view drop util.Random.nextInt(emptyCells.size) head

  def next(snakeDir: Direction = this.snakeDir): Option[World] = {
    val nextSnakeTail = snake dropRight 1
    val nextSnakeHead = snake.head moved snakeDir
    if ((nextSnakeTail contains nextSnakeHead) || (World.walls contains nextSnakeHead))
      None
    else if (nextSnakeHead == applePos)
      Some(World(nextSnakeHead::snake, snakeDir, newApplePos))
    else
      Some(World(nextSnakeHead::nextSnakeTail, snakeDir, applePos))
  }
}

object World {
  val WIDTH = 25
  val HEIGHT = 17

  val walls: Set[Pos] = (
    (0 until WIDTH map (Pos(_, HEIGHT - 1))) ++
    (0 until WIDTH map (Pos(_, 0))) ++
    (0 until HEIGHT map (Pos(0, _))) ++
    (0 until HEIGHT map (Pos(WIDTH - 1, _)))
  ).toSet

  val floors: Set[Pos] = (
    for (x <- 1 until WIDTH - 1; y <- 1 until HEIGHT - 1)
    yield Pos(x, y)
  ).toSet

  def newWorld: World = World(
    snake = 14 to 10 by -1 map (Pos(_, 10)) toList,
    snakeDir = Right,
    applePos = Pos(10, 5)
  )
}

object Main extends App {
  var world: World = World.newWorld
  var state: State = OnGoing
  val gridSize = 30
  val snakeSpeed = 200
  
  def draw(g: Graphics, edgeColor: Color, fillColor: Color)(pos: Pos): Unit = {
    g setColor fillColor
    g.fillRect(pos.x * gridSize, pos.y * gridSize, gridSize, gridSize)
    g setColor edgeColor
    g.drawRect(pos.x * gridSize, pos.y * gridSize, gridSize, gridSize)
  }

  def updateWorld(w: Option[World]): Unit = synchronized {
    w match {
      case Some(w) =>
        if (state != OnGoing) state = OnGoing
        world = w
        scoreLabel setText (w.snake.length - 5).toString
      case None =>
        state = Over
        val score = scoreLabel.getText
        scoreLabel setText s"GAME GAME  score:$score (n)ew (q)uit"
    }
  }

  val tick = new Timer(snakeSpeed, new AbstractAction() {
    def actionPerformed(e: ActionEvent): Unit = {
      if (state == OnGoing) {
        updateWorld(world.next())
      }
    }
  })

  val keyListener = new KeyListener {
    def keyPressed(e: KeyEvent) = ()
    def keyReleased(e: KeyEvent) = ()

    val commandOf = Command.fromKeyChar.lift
    def keyTyped(e: KeyEvent): Unit =
      commandOf(e.getKeyChar) foreach (_ match {
        case NewGame => updateWorld(Some(World.newWorld))
        case Quit => System.exit(0)
        case dir: Direction if state == OnGoing =>
          val curDir = world.snakeDir
          if (dir != curDir && dir != curDir.reversed) {
            tick.stop()
            updateWorld(world next dir)
            tick.restart()
          }
        case _ => ()
      })
  }

  val panel = new JPanel {
    locally {
      addKeyListener(keyListener)
      setFocusable(true)
      setBackground(Color.GRAY)
      setSize(new Dimension(gridSize * World.WIDTH, gridSize * World.HEIGHT))
      repainter.start()
    }

    def repainter = new Timer(50, new AbstractAction() {
      def actionPerformed(e: ActionEvent): Unit = {
        repaint()
      }
    })

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      World.walls foreach draw(g, Color.BLACK, Color.BLACK)
      World.floors foreach draw(g, Color.BLACK, Color.GRAY)
      world.snake foreach draw(g, Color.ORANGE, Color.YELLOW)
      world.applePos tap draw(g, Color.RED, Color.RED)
    }
  }

  val scoreLabel = new JLabel("Snake") {
    locally {
      setForeground(Color.BLUE)
      val curFont = getFont
      setFont(new Font(curFont.getName, Font.BOLD, 30))
    }
  }

  val topFrame = new JFrame() {
    locally {
      setTitle("snake.scala")
      setLayout(null)
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      val pane = getContentPane
      pane add panel
      pane add scoreLabel
      scoreLabel.setBounds(0, panel.getHeight, panel.getWidth, gridSize)
      setSize(new Dimension(panel.getWidth, panel.getHeight + scoreLabel.getHeight*2))
      setVisible(true)
    }
  }

  tick.start()
}
