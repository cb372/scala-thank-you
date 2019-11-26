package thankyou

import twitter4j._
import twitter4j.conf.ConfigurationBuilder

import scala.collection.JavaConverters._
import scala.annotation.tailrec

import cats._
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.map._
import cats.instances.set._

import guru.nidi.graphviz.engine._
import guru.nidi.graphviz.attribute._
import guru.nidi.graphviz.model.Factory._

object Main extends App {

  @tailrec
  def search(twitter: Twitter, query: Query, acc: List[Status]): List[Status] = {
    val result = twitter.search(query)
    val updatedAcc = result.getTweets().asScala.toList ++ acc
    if (result.nextQuery == null)
      updatedAcc
    else
      search(twitter, result.nextQuery, updatedAcc)
  }

  def processTweet(tweet: Status): Map[Twitterer, Set[Twitterer]] = {
    val tweetAuthor = Twitterer(tweet.getUser.getScreenName)
    val mentions = tweet.getUserMentionEntities
      .map(mention => Twitterer(mention.getScreenName()))
      .toSet
    Map(tweetAuthor -> mentions)
  }

  val twitter = {
    val config = new ConfigurationBuilder()
      .setOAuthConsumerKey(sys.env("CONSUMER_KEY"))
      .setOAuthConsumerSecret(sys.env("CONSUMER_SECRET"))
      .setOAuthAccessToken(sys.env("ACCESS_TOKEN"))
      .setOAuthAccessTokenSecret(sys.env("ACCESS_TOKEN_SECRET"))
      .build()
    new TwitterFactory(config).getInstance()
  }

  println(s"Fetching all #ScalaThankYou tweets as of ${java.time.Instant.now()}...")
  val tweets = search(twitter, new Query("#ScalaThankYou"), Nil)
  println(s"Fetched ${tweets.size} tweets")

  val relationships: Map[Twitterer, Set[Twitterer]] =
    tweets.foldMap(processTweet)

  println(s"${relationships.size} people have thanked ${relationships.map(_._2.size).sum} people (${relationships.map(_._2).flatten.toSet.size} distinct)")

  println("Building graph...")
  val graph = mutGraph("scala-thank-you").setDirected(true).use((gr, ctx) => {
    for {
      (thanker, thankees) <- relationships
      thankee <- thankees
    } {
      mutNode(thanker.toString).add("href", thanker.toUrl)
        .addLink(mutNode(thankee.toString).add("href", thankee.toUrl))
    }
  })
  println("Finished building graph")

  println("Rendering as SVG...")
  Graphviz.fromGraph(graph).render(Format.SVG).toFile(new java.io.File("scala-thank-you.svg"))
  println("Finished rendering as SVG")
}
