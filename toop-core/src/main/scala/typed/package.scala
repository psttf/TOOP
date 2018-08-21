import fastparse.WhitespaceApi

package object typed {
  val SigmaApi: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
}
