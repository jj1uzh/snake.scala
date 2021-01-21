import scala.util.chaining.scalaUtilChainingOps
import scala.language.postfixOps
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.concurrent.duration.DurationInt
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}
import javax.swing._
import java.awt.event.{KeyEvent, KeyListener, ActionEvent}
import java.awt.event.KeyEvent._
import java.awt.{Font, Graphics, Color, Rectangle, Dimension}
import java.util.concurrent.LinkedBlockingQueue

sealed trait Direction {
  def reversed: Direction = this match {
    case Up => Down
    case Right => Left
    case Left => Right
    case Down => Up
  }
}

case object Up extends Direction
case object Right extends Direction
case object Left extends Direction
case object Down extends Direction

object Direction{
  def fromKeyCode(n: Int): Option[Direction] = n match {
    case VK_DOWN => Some(Down)
    case VK_LEFT => Some(Left)
    case VK_RIGHT => Some(Right)
    case VK_UP => Some(Up)
    case _ => None
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
    if ((nextSnakeTail contains nextSnakeHead) ||
        (World.walls contains nextSnakeHead)) {
      None
    } else if (nextSnakeHead == applePos) {
      Some(World(
        nextSnakeHead::snake, snakeDir, newApplePos
      ))
    } else {
      Some(World(
        nextSnakeHead::nextSnakeTail, snakeDir, applePos
      ))
    }
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
  import ExecutionContext.Implicits.global
  var world: World = World.newWorld
  var state: State = OnGoing
  def gridSize = 30
  def snakeSpeed = 200
  def refreshInterval = 50
  
  def draw(g: Graphics, edgeColor: Color, fillColor: Color)(pos: Pos): Unit = {
    g setColor fillColor
    g.fillRect(pos.x * gridSize, pos.y * gridSize, gridSize, gridSize)
    g setColor edgeColor
    g.drawRect(pos.x * gridSize, pos.y * gridSize, gridSize, gridSize)
  }

  def panel = new JPanel() with KeyListener {
    locally {
      addKeyListener(this)
      setFocusable(true)
      setBackground(Color.GRAY)
      setSize(new Dimension(gridSize * World.WIDTH, gridSize * World.HEIGHT))
      repainter.start()
    }
    def repainter = new Timer(refreshInterval, new AbstractAction() {
      def actionPerformed(e: ActionEvent): Unit = {
        repaint()
      }
    })
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val w = world
      World.walls foreach draw(g, Color.BLACK, Color.BLACK)
      World.floors foreach draw(g, Color.BLACK, Color.GRAY)
      w.snake foreach draw(g, Color.ORANGE, Color.YELLOW)
      w.applePos tap draw(g, Color.RED, Color.RED)
    }
    def keyReleased(e: KeyEvent) = ()
    def keyTyped(e: KeyEvent) = onKeyTyped(e)
    def keyPressed(e: KeyEvent): Unit = onKeyEvent(e)
  }

  val scoreLabel = new JLabel {
    locally {
      setText("Hello")
      setForeground(Color.BLUE)
      val curFont = getFont
      setFont(new Font(curFont.getName, Font.BOLD, 20))
    }
  }

  def top = new JFrame() {
    locally {
      setTitle("snake.scala")
      setLayout(null)
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      val pane = getContentPane
      pane add panel
      pane add scoreLabel
      scoreLabel.setBounds(0, panel.getHeight, panel.getWidth, gridSize)
      setSize(new Dimension(panel.getWidth, panel.getHeight + scoreLabel.getHeight*2))
    }
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

  def onKeyEvent(ev: KeyEvent): Unit = {
    val snakeDir = world.snakeDir
    for (
      dir <- Direction fromKeyCode ev.getKeyCode;
      if dir != snakeDir.reversed & dir != snakeDir & state == OnGoing
    ) {
      tick.stop()
      updateWorld(world next dir)
      tick.restart()
    }
  }

  def onKeyTyped(ev: KeyEvent): Unit = {
    tick.stop()
    ev.getKeyChar match {
      case 'n' => updateWorld(Some(World.newWorld))
      case 'q' => System.exit(0)
    }
    tick.restart()
  }

  val tick = new Timer(snakeSpeed, new AbstractAction() {
    def actionPerformed(e: ActionEvent): Unit = {
      if (state == OnGoing) {
        updateWorld(world.next())
      }
    }
  })

  top setVisible true
  tick.start()
}
