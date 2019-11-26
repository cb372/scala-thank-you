package thankyou

case class Twitterer(handle: String) {
  override def toString: String = s"@$handle"
  def toUrl: String = s"https://twitter.com/$handle"
}

