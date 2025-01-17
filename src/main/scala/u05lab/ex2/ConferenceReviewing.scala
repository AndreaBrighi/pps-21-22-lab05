package u05lab.ex2

enum Question:
  case RELEVANCE // ("È importante per questa conferenza?"),
  case SIGNIFICANCE // ("Produce contributo scientifico?"),
  case CONFIDENCE // ("Ti senti competente a commentarlo?");
  case FINAL   // ("É un articolo da accettare?")

class ConferenceReviewing:

  private var articles = Map[Int, List[Map[Question, Int]]]()

  /**
   * @param article
   * @param scores
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    articles = articles + (article -> (scores :: articles.getOrElse(article, List())))


  /**
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    articles = articles +
      (article -> (Map(Question.RELEVANCE -> relevance, Question.SIGNIFICANCE -> significance, Question.CONFIDENCE -> confidence, Question.FINAL -> fin)
        :: articles.getOrElse(article, List())))

  /**
   * @param article
   * @param question
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Int, question: Question): List[Int] = articles(article).flatMap(_.filter(_._1 == question).values).sorted

  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double =
    mean(articles(article).map(_(Question.FINAL)))

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles: Set[Int] =
    val averageThreshold = 5
    val relevanceThreshold = 8
    articles.filter(article => mean(article._2.map(_(Question.FINAL))) > averageThreshold && article._2.map(_(Question.RELEVANCE)).exists(_ >= relevanceThreshold))
    .keySet


  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Int, Double)] = articles.filter(article => acceptedArticles.contains(article._1)).map(article => (article._1, averageFinalScore(article._1))).toList.sortWith(_._2 < _._2)

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap: Map[Int, Double] =
    val maxConfidence = 10.0
    articles.map(a => (a._1, mean(a._2.map(q => q(Question.CONFIDENCE) * q(Question.FINAL) / maxConfidence))))

  private def mean(list: List[Double]): Double = list.sum / list.size