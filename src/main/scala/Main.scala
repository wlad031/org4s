package dev.vgerasimov.org4s

import models.elements.Paragraph
import models.{Element, Section}
import optic._

import dev.vgerasimov.org4s.models.objects.Link
import dev.vgerasimov.org4s.models.objects.Link.{PlainLink, Protocol}
import dev.vgerasimov.org4s.models.objects.Link.RegularLink.ProtocolLink
import fastparse._
import pl.codeset.pocket.add.AddItemCmd
import pl.codeset.pocket.read.ItemState
import pl.codeset.pocket.PocketAuthFactory
import pl.codeset.pocket.Pocket
import pl.codeset.pocket.PocketAuth
import pl.codeset.pocket.read.GetItemsCmd
import pl.codeset.pocket.read.GetItemsResult
import pl.codeset.pocket.read.PocketItem
import ops._

import scala.io.Source
import scala.jdk.CollectionConverters.IterableHasAsJava

object Main {
  case class Article(title: String, link: String, tags: List[String])

  val toArticleMapper: Mapper[Section, Article] = new Mapper[Section, Article] {
    override def map(section: Section): Article = {
      Article(
        {
          val title = section.headline.title.get
          title.contents.mkString.strip()
        }, {
        val link = section.elements.head.asInstanceOf[Paragraph].objects.head.asInstanceOf[Link]
          link match {
            case ProtocolLink(Protocol(pr), path, _) => s"$pr://$path"
            case PlainLink(Protocol(pr), path) => s"$pr://$path"
            case x => sys.error(x.toString)
          }
      },
          section.headline.tags
      )
    }
  }

  def main1(args: Array[String]): Unit = {




//    val factory = PocketAuthFactory.create(consumerKey, "https://getpocket.com/")
//    val authCode = factory.getCode
//    val authorizationUrl = factory.getAuthUrl
//
//    println(authorizationUrl)

//    val pocketAuth = PocketAuthFactory.createForAccessToken(consumerKey, accessToken)
//    val pocket = new Pocket(pocketAuth)
//    val accessToken = pocketAuth.getAccessToken
//

//    val cmd = new GetItemsCmd.Builder().state(ItemState.all).count(5).build
//    var getResult = pocket.getItems(cmd)
//    var items = getResult.getList
//
//    println(items)

//    pocket.addItem(
//      new AddItemCmd.Builder("https://my-bookmark-url.com")
//        .tags(util.Arrays.asList("tag1", "tag2"))
//        .title("My bookmark title")
//        .build()
//    );

//    getResult = pocket.getItems(cmd)
//    items = getResult.getList
//
//    println(items)
  }



  def main(args: Array[String]): Unit = {
    println("hello")

//    val filename = "/Users/vgerasimov/Desktop/reading-short.org"
    val filename = "/Users/vgerasimov/org/reading.org"
//    val filename = "/Users/vgerasimov/org/recipies.org"
//    val filename = "/Users/vgerasimov/org/reading.org"

    val fileContents = Source.fromFile(filename).getLines().mkString("\n")

    parse(fileContents, new OrgParser().document(_)) match {
      case failure: Parsed.Failure =>
        println(failure)
      case Parsed.Success(value, index) =>
//        println(value)

//        value
//          .sections(1)
//          .childSections
//          .filter(cs => cs.elements.nonEmpty && cs.elements(0).isInstanceOf[Paragraph] && cs.elements(0).asInstanceOf[Paragraph].objects(0).isInstanceOf[Link.RegularLink.ProtocolLink])
//          .map(x =>
//            Article(
//              x.headline.title.get.contents.toString().strip(), {
//                val link = x.elements(0).asInstanceOf[Paragraph].objects(0).asInstanceOf[RegularLink.ProtocolLink]
//                s"${link.protocol.value}://${link.path}"
//              },
//              x.headline.tags
//            )
//          )
//          .foreach(x => println(s"A: $x"))

        val k = DocumentMapper.headSection.map(
          DocumentFilter.sections(
            SectionMatcher.byHeadline(
              HeadlineMatcher.byTitle(
                HeadlineTitleMatcher.caseInsensitiveSubstring("article").option()
              )
            )
          )
          .filter(value))
          .map(_.childSections)
          .map(_.filter(!_.headline.hasCommentKeyword).map(toArticleMapper.map))

        val articles = k.get
//        println(k.get.mkString("\n"))

        val consumerKey = "98034-0492f093f549383386e2e0e9"
        val accessToken = "5c934298-c72d-fcd1-a94d-73f896"

//        val factory = PocketAuthFactory.create(consumerKey, "https://getpocket.com/")
//        val authCode = factory.getCode
//        val authorizationUrl = factory.getAuthUrl
//
//        println(authorizationUrl)

        val pocketAuth = PocketAuthFactory.createForAccessToken(consumerKey, accessToken)
        val pocket = new Pocket(pocketAuth)
//        val accessToken = pocketAuth.getAccessToken

//        val cmd = new GetItemsCmd.Builder().state(ItemState.all).count(5).build
//        var getResult = pocket.getItems(cmd)
//        var items = getResult.getList
//
//        println(items)

        articles
          .map(article => {
            println("-".repeat(80))
            println(s"Article : $article")
            val cmd = new AddItemCmd.Builder(article.link)
              .tags(new java.util.ArrayList(article.tags.asJavaCollection))
              .title(article.title)
              .build()
            cmd
          })
          .foreach(cmd => {
            val res = pocket.addItem(cmd)
            println(s"Result : $res")
            Thread.sleep(100)
          })

//        pocket.addItem(
//          new AddItemCmd.Builder("https://my-bookmark-url.com")
//            .tags(java.util.Arrays.asList("tag1", "tag2"))
//            .title("My bookmark title")
//            .build()
//        );
    }
  }
}
