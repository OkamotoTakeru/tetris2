/*
プログラムの実行手順：
1. ターミナル / コマンドプロンプトを開く
2. build.sbt が置かれた場所で sbt と入力し、return を押す
3. project tetris と入力し、return を押す
4. run と入力し、return を押す
5. コンパイルが成功したら、tetris.A を選択（1 と入力）し、return を押す
6. ゲーム画面を閉じたら、手動で java を終了する
7. プログラムを変更後、もう一度実行したいときは run と入力し、return を押す
*/

package tetris

import scala.util.Random

import sgeometry.Pos
import sdraw.{World, Color, Transparent, HSB}

import tetris.{ShapeLib => S}
import colors.Blue



// テトリスを動かすための関数
case class TetrisWorld(piece: ((Int, Int), S.Shape), pile: S.Shape) extends World() {

  // マウスクリックは無視
  def click(p: sgeometry.Pos): World = this

  // ブロックの描画
  def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color): Boolean = {
    canvas.drawRect(Pos(A.BlockSize * x, A.BlockSize * y), A.BlockSize * w, A.BlockSize * h, c)
  }

  // shape の描画（与えられた位置）
  def drawShape(pos: (Int, Int), shape: S.Shape): Boolean = {
    val pos_colors = shape.zipWithIndex.flatMap(row_i => {
      val (row, i) = row_i
      row.zipWithIndex.map(box_j => {
        val (color, j) = box_j
        (j, i, color)
      })
    })

    val (x, y) = pos
    pos_colors.forall(pos_color => {
      val (dx, dy, color) = pos_color
      drawRect(x + dx, y + dy, 1, 1, color)
    })
  }

  // shape の描画（原点）
  def drawShape00(shape: S.Shape): Boolean = drawShape((0, 0), shape)

  // ゲーム画面の描画
  val CanvasColor = HSB(0, 0, 0.1f)

  def draw(): Boolean = {
    val (pos, shape) = piece
    canvas.drawRect(Pos(0, 0), canvas.width, canvas.height, CanvasColor) &&
    drawShape00(pile) &&
    drawShape(pos, shape)
  }

  // 1, 4, 7. tick
  /*
  // 1.tick
  // 目的：テトロミノを１ずつ下に落下させる(突き抜けて構わない)
  def tick(): World = {
    val ((x,y),shape) = piece
    val p = ((x,y+1),shape)
    TetrisWorld(p, pile)
  }
  */
  /*
  // 4.tick
  // 目的：tickをテトロミノが一番下まで来たらそれ以上落下しないようにする改良する
  def tick(): World = {
    val ((x,y),shape) = piece
    val p = ((x,y+1),shape)
    val w = TetrisWorld(p, pile)
    if (collision(w)) TetrisWorld(piece,pile) else w
  }
  */

  

  // 7.tick
  // 目的：tickをテトロミノが下に移動できなくなったときに適切な処理をするように改良する
  def tick(): World= {
    val world = TetrisWorld(piece,pile)
    val ((x,y),shape) = piece
    val p = ((x,y+1),shape)
    val w = TetrisWorld(p, pile) 
    if(!A.endflag){
      if (collision(w)){ //一番下まで落ちる分岐
        val combinedpile = S.combine(S.shiftSE(shape,x,y),pile)
        A.sc = A.sc + score(combinedpile)
        val z = TetrisWorld(A.nextpiece,eraseRows(combinedpile))
        A.nextpiece = A.newPiece() 
        val ((a,b),np) = A.nextpiece
        val ((c,d),h) = A.hold
        println("SCORE: " + A.sc + "  NEXT: " + shapename(np) + " HOLD: " + shapename(h))
        if (collision(z)){ //ゲームが終わる分岐
          A.endflag = true
          println("GAME OVER!")
          world
        }
        else z
      }
      else w
    }
    else world
  }
  
  // 2, 5. keyEvent
  /*
  // 2. keyEvent
  // 目的：キー入力に応じてテトロミノを操作する
  def keyEvent(key: String): World = {
    val ((x,y),shape) = piece
    var p = piece
    key match{
      case "RIGHT" => p = ((x+1,y),shape)
      case "LEFT"  => p = ((x-1,y),shape)
      case "UP"    => p = ((x,y),S.rotate(shape))
    }
    TetrisWorld(p, pile)
  }
  */
  // 5. keyEvent
  // 目的：キー操作によって衝突が起こるときその操作を無視するようにkeyEventを改良する
  def keyEvent(key: String): World = {
    val world = TetrisWorld(piece,pile)
    var ((x,y),shape) = piece
    var p = ((x,y),shape)
    key match{
      case "RIGHT" => p = ((x+1,y),shape)
      case "LEFT"  => p = ((x-1,y),shape)
      case "UP"    => p = ((x,y),S.rotate(shape))
      case "DOWN"  => p = ((x,y+1),shape)
      case "SHIFT" => {
        var s = S.random()
        if(!collision(TetrisWorld(A.hold,pile)))
        s = A.hld
        A.hld = shape
        shape = s
        A.hold = ((x,y),A.hld)
        p = ((x,y),shape)
      }
    }
    if(!A.endflag){
      if(collision(TetrisWorld(p,pile))) TetrisWorld(piece, pile)
      else TetrisWorld(p,pile)
    }
    else world
  }

  // 3. collision
  // 目的：受け取った世界が衝突を起こしているかを調べる
  def collision(world: TetrisWorld): Boolean = {
    val TetrisWorld(piece,pile) = world
    val ((x,y),shape) = piece
    val (m,n) = S.size(shape)
    x < 0 || x + n > A.WellWidth || y + m > A.WellHeight || S.overlap(pile,S.shiftSE(shape,x,y))
  }
  
  // 6. eraseRows
  // 目的：揃った行を消去する
  def eraseRows(pile: S.Shape): S.Shape = {
    S.empty(A.WellHeight-pile.count(rowcheck),A.WellWidth)++pile.filter(rowcheck)
  }

  def rowcheck(row:S.Row):Boolean ={
      row.contains(Transparent) || row.length != A.WellWidth
    }
 
  // EX
  def shapename(shape:S.Shape):String={
    var str = "　"
    val shapej2 = S.rotate(S.shapeJ)
    val shapej3 = S.rotate(shapej2)
    val shapej4 = S.rotate(shapej3)
    val shapet2 = S.rotate(S.shapeT)
    val shapet3 = S.rotate(shapet2)
    val shapet4 = S.rotate(shapet3)
    val shapel2 = S.rotate(S.shapeL)
    val shapel3 = S.rotate(shapel2)
    val shapel4 = S.rotate(shapel3)
    val shapes2 = S.rotate(S.shapeS)
    val shapei2 = S.rotate(S.shapeI)
    val shapez2 = S.rotate(S.shapeZ)

    shape match{
      case Nil => str = "  "
      case S.shapeJ => str = "J"
      case S.shapeT => str = "T"
      case S.shapeO => str = "O"
      case S.shapeL => str = "L"
      case S.shapeS => str = "S"
      case S.shapeI => str = "I"
      case S.shapeZ => str = "Z"
      case shapej2 => str = "J"
      case shapej3 => str = "J"
      case shapej4 => str = "J"
      case shapet2 => str = "T"
      case shapet3 => str = "T"
      case shapet4 => str = "T"
      case shapel2 => str = "L"
      case shapel3 => str = "L"
      case shapel4 => str = "L"
      case shapes2 => str = "S"
      case shapei2 => str = "I"
      case shapez2 => str = "Z"
      case _ => str = "  "
    }
    str
  }

  def score(pile:S.Shape):Int={
    var s = A.WellHeight - pile.count(rowcheck)
    s match{
      case 0 => 0
      case 1 => 40
      case 2 => 100
      case 3 => 300
      case 4 => 1200
      case _ => 0
    }
  }

}

// ゲームの実行
object A extends App {
  // ゲームウィンドウとブロックのサイズ
  val WellWidth = 10
  val WellHeight = 10
  val BlockSize = 30

  // 新しいテトロミノの作成
  val r = new Random()

  def newPiece(): ((Int, Int), S.Shape) = {
    val pos = (WellWidth / 2 - 1, 0)
    (pos,
     List.fill(r.nextInt(4))(0).foldLeft(S.random())((shape, _) => shape))
  }

  // 最初のテトロミノ
  val piece = newPiece()
  //その次の予告用ミノ
  var nextpiece = newPiece()
  //ホールド
  var hold = newPiece()
  var ((cc,dd),hld)= hold

  // ゲームの初期値
  val world = TetrisWorld(piece, List.fill(WellHeight)(List.fill(WellWidth)(Transparent)))
  
  //初期スコア
  var sc = 0

  //初期のスコアと予告ピノ
  def shapename(shape:S.Shape):String={
    var str = "　"
    shape match{
      case Nil => str = "  "
      case S.shapeJ => str = "J"
      case S.shapeT => str = "T"
      case S.shapeO => str = "O"
      case S.shapeL => str = "L"
      case S.shapeS => str = "S"
      case S.shapeI => str = "I"
      case S.shapeZ => str = "Z"
      case _ => str = "  "
    }
    str
  }
  val ((aa,bb),shp) = nextpiece

  println("SCORE: " + sc + "  NEXT: " + shapename(shp) + " HOLD: " + shapename(hld))

  //終わりのフラグ
  var endflag = false
  
  // ゲームの開始
  world.bigBang(BlockSize * WellWidth, BlockSize * WellHeight, 1)
  
}
