import edu.stanford.nlp.tagger.maxent.MaxentTagger
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by on 7/28/2015.
 */
object wordStatistics {
  def main(args: Array[String]) {
    // download the pos tagger
    // http://www.galalaly.me/index.php/2011/05/tagging-text-with-stanford-pos-tagger-in-java-applications/
    // Initialize the tagger
    val tagger = new MaxentTagger(
      "lib/stanford-postagger/models/left3words-wsj-0-18.tagger");

    // The sample string
    val sample = "As two-toned as the city that serves as its title, backdrop and subject matter, Oscar Luis Costo’s “Shanghai Red” combines a “Bride Wore Black”-type revenge drama (except here the widow favors red) with strong meller elements. Pic evolves into a struggle between genres as Vivian Wu’s heroine insists on relating her tale as a complex love story while her straight-arrow lawyer reads it as simple crime thriller. Neither strain is especially strong (major plot reversals are blindingly predictable), but well-thesped tensions prove oddly fascinating. Elegant, dreamy actioner lite has an outside shot at theatrical distribution.\nMei Li (Wu, who co-produced with spouse Costo), clad in a crimson sheath and dark glasses, engages in a round of sophisticated sexual banter before blowing away one of the fat cats allegedly involved in her husband’s death. She then changes back to a blouse and skirt and reassumes her identity as a mild-mannered translator and dedicated mother of an 8-year-old son.\nMei Li is caught between the blood lust of her husband’s sinister ex-partner Fong (Ge Wu), on whose say-so she executes her vengeful hits, and her passionate surrender to a new love interest, Michael Johnson (Richard Burgi), an American “facilitator” who hires her for her translating skills. She remains haunted by visions of her dead husband who mournfully materializes wherever she goes.\nThe thoughts that both Fong and Johnson may be advancing their own agendas and that Mei Li may be enmeshed in a web of treachery and betrayal occur to her several beats later than they occur to the audience. But, by then, the context has shifted.\n“Shanghai Red” unfolds in various time-frames, helmer Costo freely intercutting between them: There’s the present-tense of the film’s action, flashbacks to Mei Lei’s fond past with her husband, and the aftermath which finds Mei Li, now with cropped hair and garbed in prison gray, justifying her acts to her court-appointed lawyer (Sun Hong-Lei).\nIntense perfs by Sun and particularly Wu, who illuminates all three of the very distinct stages of her character’s progression and the learning curve that links them, overlay the otherwise conventional revenge story with a highly-charged conflict over its ultimate meaning.\nIn many ways, Costo’s film provides its own correctives, taking unexpected detours at all the plot’s weakest, most predictable moments, and thereby defusing action-movie cliches. Thus the “hit man in love with his target” chestnut gets completely sidetracked when Mei Li suddenly reveals that her deeds are based on guilt rather than on love.\nShanghai itself develops into a character as widescreen vistas of the city are woven into the narrative. Costo links the deadly corporate hanky-panky to the new Shanghai with its soaring Western skyscrapers and streamlined amorality that is inexorably replacing the quaint byways of a less expendable civilization.\nTech credits are impressive.";

    // The tagged string
    val tagged = tagger.tagString(sample);

    // Output the result
    System.out.println(tagged);

    // 统计整体词频
    val words1 = tagged.split(" ").map(w => (w, 1)).groupBy(w => w._1).map(ws => {
      (ws._1, ws._2.size)
    })
    words1.foreach(w => {
      println(w)
    })

    // 按照词性进行词频统计
    val words2 = tagged.split(" ").map(w => {
      var tmp = w.split("/")
      (tmp(1), tmp(0))
    }).groupBy(w => {
      w._1
    }).map(w => {
      val grouped = w._2.map(k => k._2).groupBy(w => w).map(k => (k._1, k._2.size)).toSeq.sortBy(k => k._2).reverse
      val mapped = grouped.map(k => ((k._1 + "/" + w._1), k._2))
      mapped
    })
    val iter = words2.iterator
    while(iter.hasNext) {
      var iter2 = iter.next().iterator
      while(iter2.hasNext) {
        println(iter2.next())
      }
    }
  }

  // 整体进行词频统计()

  def statisticWholeContent(sample: String, tagger: MaxentTagger): Unit = {
    val tagged = tagger.tagString(sample);
    val words1 = tagged.split(" ").map(w => (w, 1)).groupBy(w => w._1).map(ws => {
      (ws._1, ws._2.size)
    })
    words1.foreach(w => {
      println(w)
    })
  }

  // 按照词性进行词频统计
  def statisticBasedOnWordsFreq(sample: String, tagger: MaxentTagger): Unit ={
    val tagged = tagger.tagString(sample);
    // 统计整体词频
    val words1 = tagged.split(" ").map(w => (w, 1)).groupBy(w => w._1).map(ws => {
      (ws._1, ws._2.size)
    })
    words1.foreach(w => {
      println(w)
    })
    // 按照词性进行词频统计
    val words2 = tagged.split(" ").map(w => {
      var tmp = w.split("/")
      (tmp(1), tmp(0))
    }).groupBy(w => {
      w._1
    }).map(w => {
      val grouped = w._2.map(k => k._2).groupBy(w => w).map(k => (k._1, k._2.size)).toSeq.sortBy(k => k._2).reverse
      val mapped = grouped.map(k => ((k._1 + "/" + w._1), k._2))
      mapped
    })
    val iter = words2.iterator
    while(iter.hasNext) {
      var iter2 = iter.next().iterator
      while(iter2.hasNext) {
        println(iter2.next())
      }
    }
  }

  def singleYingping(rootPath: String, tagger: MaxentTagger): Unit = {
    val rootDir = new File(rootPath)
    val these = rootDir.listFiles()
    these.map(file => {
      (file.getName)
    }).flatMap(fname => {
      var tmpContent = new ArrayBuffer[String]
      var tmpYingping: String = null
      var tmp: String = null
      val iter = Source.fromFile(fname).getLines()
      while(iter.hasNext) {
        tmp = iter.next()
        if(tmp.contains("###")) {
          tmpContent += tmpYingping
          tmpYingping = ""
        } else {
          tmpYingping += tmp
        }
      }
      tmpContent
    }).foreach(yingping => {
      statisticWholeContent(yingping, tagger)
      statisticBasedOnWordsFreq(yingping, tagger)
    })
  }

  def singleDianying(rootPath: String, tagger: MaxentTagger): Unit ={
    val rootDir = new File(rootPath)
    val these = rootDir.listFiles()
    these.map(file => {
      (file.getName)
    }).map(fname => {
      var tmpContent: String = null
      tmpContent = Source.fromFile(fname).mkString
      statisticWholeContent(tmpContent, tagger)
      statisticBasedOnWordsFreq(tmpContent, tagger)
    })
  }

  def allDianying(rootPath: String, tagger: MaxentTagger): Unit = {
    val rootDir = new File(rootPath)
    val these = rootDir.listFiles()
    val wholeContent = these.map(file => {
      (file.getName)
    }).map(fname => {
      var tmpContent: String = null
      tmpContent = Source.fromFile(fname).mkString
      tmpContent
    }).reduce((a, b) => a + b)
    statisticBasedOnWordsFreq(wholeContent, tagger)
    statisticWholeContent(wholeContent, tagger)
  }
}
