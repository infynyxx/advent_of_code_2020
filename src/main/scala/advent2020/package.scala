import scala.io.Source

package object advent2020 {
  def inputToVector(fileName: String) = {
    Source.fromResource(fileName).getLines().toVector
  }
}
