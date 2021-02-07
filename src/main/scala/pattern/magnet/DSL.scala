package pattern.magnet

object DSL {
  def complete(magnet: CompletionMagnet): magnet.Result = magnet()
}
