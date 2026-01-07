import gleam/bit_array
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import news_and_research

pub fn main() {
  gleeunit.main()
}

// Test 1: News Bulletin Types
pub fn news_bulletin_type_test() {
  let general = news_and_research.GeneralNews
  let regulatory = news_and_research.Regulatory
  let corporate = news_and_research.CorporateAction
  let market = news_and_research.MarketEvent
  let analyst = news_and_research.AnalystComment

  news_and_research.format_bulletin_type(general)
  |> should.equal("GENERAL NEWS")

  news_and_research.format_bulletin_type(regulatory)
  |> should.equal("REGULATORY")

  news_and_research.format_bulletin_type(corporate)
  |> should.equal("CORPORATE ACTION")

  news_and_research.format_bulletin_type(market)
  |> should.equal("MARKET EVENT")

  news_and_research.format_bulletin_type(analyst)
  |> should.equal("ANALYST COMMENT")
}

// Test 2: News Bulletin Creation
pub fn news_bulletin_creation_test() {
  let bulletin =
    news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "Market opens higher today",
      exchange: "NYSE",
    )

  bulletin.msg_id
  |> should.equal(1001)

  bulletin.message
  |> should.equal("Market opens higher today")

  bulletin.exchange
  |> should.equal("NYSE")
}

// Test 3: News Article Creation
pub fn news_article_creation_test() {
  let article =
    news_and_research.NewsArticle(
      request_id: 1001,
      article_id: "ART12345",
      title: "Tech stocks rally",
      author: "John Doe",
      pub_date: "2024-01-07 10:00:00",
      summary: "Technology stocks surge",
      url: "https://example.com/article",
    )

  article.article_id
  |> should.equal("ART12345")

  article.title
  |> should.equal("Tech stocks rally")

  article.author
  |> should.equal("John Doe")
}

// Test 4: Research Article Creation
pub fn research_article_creation_test() {
  let article =
    news_and_research.ResearchArticle(
      request_id: 1001,
      article_id: "RES12345",
      title: "AAPL Buy Rating",
      author: "Analyst Team",
      pub_date: "2024-01-07 10:00:00",
      summary: "Strong earnings expected",
      rating: "BUY",
      target_price: 200.0,
    )

  article.article_id
  |> should.equal("RES12345")

  article.rating
  |> should.equal("BUY")

  article.target_price
  |> should.equal(200.0)
}

// Test 5: Request News Bulletins Message
pub fn request_news_bulletins_test() {
  let message = news_and_research.request_news_bulletins(True)

  message
  |> bit_array.byte_size()
  |> should.equal(2)

  let message2 = news_and_research.request_news_bulletins(False)

  message2
  |> bit_array.byte_size()
  |> should.equal(2)
}

// Test 6: Cancel News Bulletins Message
pub fn cancel_news_bulletins_test() {
  let message = news_and_research.cancel_news_bulletins()

  message
  |> bit_array.byte_size()
  |> should.equal(1)
}

// Test 7: Request News Article Message
pub fn request_news_article_test() {
  let message = news_and_research.request_news_article(1001, "ART12345")

  let size = message |> bit_array.byte_size()
  size
  |> should.equal(7)
}

// Test 8: Request Historical News Message
pub fn request_historical_news_test() {
  let message =
    news_and_research.request_historical_news(
      1001,
      12_345,
      "BRFG",
      "2024-01-01 00:00:00",
      "2024-01-07 23:59:59",
      100,
    )

  let size = message |> bit_array.byte_size()
  size
  |> should.equal(67)
}

// Test 9: Cancel Historical News Message
pub fn cancel_historical_news_test() {
  let message = news_and_research.cancel_historical_news(1001)

  let size = message |> bit_array.byte_size()
  size
  |> should.equal(7)
}

// Test 10: Request Research Articles Message
pub fn request_research_articles_test() {
  let message =
    news_and_research.request_research_articles(1001, 12_345, "IBKR")

  let size = message |> bit_array.byte_size()
  size
  |> should.equal(20)
}

// Test 11: Cancel Research Articles Message
pub fn cancel_research_articles_test() {
  let message = news_and_research.cancel_research_articles(1001)

  let size = message |> bit_array.byte_size()
  size
  |> should.equal(7)
}

// Test 12: Filter Bulletins by Type
pub fn filter_bulletins_by_type_test() {
  let bulletins = [
    news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 1",
      exchange: "NYSE",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1002,
      bulletin_type: news_and_research.Regulatory,
      message: "Regulation 1",
      exchange: "NYSE",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1003,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 2",
      exchange: "NASDAQ",
    ),
  ]

  let filtered =
    news_and_research.filter_bulletins_by_type(
      bulletins,
      news_and_research.GeneralNews,
    )

  list.length(filtered)
  |> should.equal(2)

  filtered
  |> list.first()
  |> should.equal(
    Ok(news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 1",
      exchange: "NYSE",
    )),
  )
}

// Test 13: Filter Bulletins by Exchange
pub fn filter_bulletins_by_exchange_test() {
  let bulletins = [
    news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 1",
      exchange: "NYSE",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1002,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 2",
      exchange: "NASDAQ",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1003,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 3",
      exchange: "NYSE",
    ),
  ]

  let filtered =
    news_and_research.filter_bulletins_by_exchange(bulletins, "NYSE")

  list.length(filtered)
  |> should.equal(2)

  filtered
  |> list.first()
  |> should.equal(
    Ok(news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 1",
      exchange: "NYSE",
    )),
  )
}

// Test 14: Search Bulletins by Keyword
pub fn search_bulletins_by_keyword_test() {
  let bulletins = [
    news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "Market rally continues",
      exchange: "NYSE",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1002,
      bulletin_type: news_and_research.GeneralNews,
      message: "Earnings report released",
      exchange: "NASDAQ",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1003,
      bulletin_type: news_and_research.GeneralNews,
      message: "Market volatility expected",
      exchange: "NYSE",
    ),
  ]

  let filtered =
    news_and_research.search_bulletins_by_keyword(bulletins, "Market")

  list.length(filtered)
  |> should.equal(2)
}

// Test 15: Get Articles by Author
pub fn get_articles_by_author_test() {
  let articles = [
    news_and_research.NewsArticle(
      request_id: 1001,
      article_id: "ART1",
      title: "Article 1",
      author: "John Doe",
      pub_date: "2024-01-07",
      summary: "Summary 1",
      url: "url1",
    ),
    news_and_research.NewsArticle(
      request_id: 1002,
      article_id: "ART2",
      title: "Article 2",
      author: "Jane Smith",
      pub_date: "2024-01-07",
      summary: "Summary 2",
      url: "url2",
    ),
    news_and_research.NewsArticle(
      request_id: 1003,
      article_id: "ART3",
      title: "Article 3",
      author: "John Doe",
      pub_date: "2024-01-07",
      summary: "Summary 3",
      url: "url3",
    ),
  ]

  let filtered = news_and_research.get_articles_by_author(articles, "John Doe")

  list.length(filtered)
  |> should.equal(2)

  filtered
  |> list.first()
  |> should.equal(
    Ok(news_and_research.NewsArticle(
      request_id: 1001,
      article_id: "ART1",
      title: "Article 1",
      author: "John Doe",
      pub_date: "2024-01-07",
      summary: "Summary 1",
      url: "url1",
    )),
  )
}

// Test 16: Get Research by Rating
pub fn get_research_by_rating_test() {
  let articles = [
    news_and_research.ResearchArticle(
      request_id: 1001,
      article_id: "RES1",
      title: "Research 1",
      author: "Analyst 1",
      pub_date: "2024-01-07",
      summary: "Summary 1",
      rating: "BUY",
      target_price: 200.0,
    ),
    news_and_research.ResearchArticle(
      request_id: 1002,
      article_id: "RES2",
      title: "Research 2",
      author: "Analyst 2",
      pub_date: "2024-01-07",
      summary: "Summary 2",
      rating: "HOLD",
      target_price: 150.0,
    ),
    news_and_research.ResearchArticle(
      request_id: 1003,
      article_id: "RES3",
      title: "Research 3",
      author: "Analyst 3",
      pub_date: "2024-01-07",
      summary: "Summary 3",
      rating: "BUY",
      target_price: 180.0,
    ),
  ]

  let filtered = news_and_research.get_research_by_rating(articles, "BUY")

  list.length(filtered)
  |> should.equal(2)

  filtered
  |> list.first()
  |> should.equal(
    Ok(news_and_research.ResearchArticle(
      request_id: 1001,
      article_id: "RES1",
      title: "Research 1",
      author: "Analyst 1",
      pub_date: "2024-01-07",
      summary: "Summary 1",
      rating: "BUY",
      target_price: 200.0,
    )),
  )
}

// Test 17: Get Research by Target Price Above Threshold
pub fn get_research_by_target_price_test() {
  let articles = [
    news_and_research.ResearchArticle(
      request_id: 1001,
      article_id: "RES1",
      title: "Research 1",
      author: "Analyst 1",
      pub_date: "2024-01-07",
      summary: "Summary 1",
      rating: "BUY",
      target_price: 200.0,
    ),
    news_and_research.ResearchArticle(
      request_id: 1002,
      article_id: "RES2",
      title: "Research 2",
      author: "Analyst 2",
      pub_date: "2024-01-07",
      summary: "Summary 2",
      rating: "HOLD",
      target_price: 150.0,
    ),
    news_and_research.ResearchArticle(
      request_id: 1003,
      article_id: "RES3",
      title: "Research 3",
      author: "Analyst 3",
      pub_date: "2024-01-07",
      summary: "Summary 3",
      rating: "BUY",
      target_price: 180.0,
    ),
  ]

  let filtered = news_and_research.get_research_by_target_price(articles, 175.0)

  list.length(filtered)
  |> should.equal(2)
}

// Test 18: Get Research Below Target Price
pub fn get_research_below_target_price_test() {
  let articles = [
    news_and_research.ResearchArticle(
      request_id: 1001,
      article_id: "RES1",
      title: "Research 1",
      author: "Analyst 1",
      pub_date: "2024-01-07",
      summary: "Summary 1",
      rating: "BUY",
      target_price: 200.0,
    ),
    news_and_research.ResearchArticle(
      request_id: 1002,
      article_id: "RES2",
      title: "Research 2",
      author: "Analyst 2",
      pub_date: "2024-01-07",
      summary: "Summary 2",
      rating: "HOLD",
      target_price: 150.0,
    ),
    news_and_research.ResearchArticle(
      request_id: 1003,
      article_id: "RES3",
      title: "Research 3",
      author: "Analyst 3",
      pub_date: "2024-01-07",
      summary: "Summary 3",
      rating: "BUY",
      target_price: 180.0,
    ),
  ]

  let filtered =
    news_and_research.get_research_below_target_price(articles, 175.0)

  list.length(filtered)
  |> should.equal(1)

  filtered
  |> list.first()
  |> should.equal(
    Ok(news_and_research.ResearchArticle(
      request_id: 1002,
      article_id: "RES2",
      title: "Research 2",
      author: "Analyst 2",
      pub_date: "2024-01-07",
      summary: "Summary 2",
      rating: "HOLD",
      target_price: 150.0,
    )),
  )
}

// Test 19: Count Bulletins by Type
pub fn count_bulletins_by_type_test() {
  let bulletins = [
    news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 1",
      exchange: "NYSE",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1002,
      bulletin_type: news_and_research.Regulatory,
      message: "Reg 1",
      exchange: "NYSE",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1003,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 2",
      exchange: "NASDAQ",
    ),
    news_and_research.NewsBulletin(
      msg_id: 1004,
      bulletin_type: news_and_research.GeneralNews,
      message: "News 3",
      exchange: "NYSE",
    ),
  ]

  let count =
    news_and_research.count_bulletins_by_type(
      bulletins,
      news_and_research.GeneralNews,
    )

  count
  |> should.equal(3)

  let count2 =
    news_and_research.count_bulletins_by_type(
      bulletins,
      news_and_research.Regulatory,
    )

  count2
  |> should.equal(1)
}

// Test 20: Format News Bulletin
pub fn format_news_bulletin_test() {
  let bulletin =
    news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "Market rally continues",
      exchange: "NYSE",
    )

  let formatted = news_and_research.format_news_bulletin(bulletin)

  formatted
  |> string.contains("Message ID: 1001")
  |> should.equal(True)

  formatted
  |> string.contains("Type: GENERAL NEWS")
  |> should.equal(True)

  formatted
  |> string.contains("Exchange: NYSE")
  |> should.equal(True)

  formatted
  |> string.contains("Message: Market rally continues")
  |> should.equal(True)
}

// Test 21: Format News Article
pub fn format_news_article_test() {
  let article =
    news_and_research.NewsArticle(
      request_id: 1001,
      article_id: "ART12345",
      title: "Tech stocks rally",
      author: "John Doe",
      pub_date: "2024-01-07 10:00:00",
      summary: "Technology stocks surge",
      url: "https://example.com/article",
    )

  let formatted = news_and_research.format_news_article(article)

  formatted
  |> string.contains("Article ID: ART12345")
  |> should.equal(True)

  formatted
  |> string.contains("Title: Tech stocks rally")
  |> should.equal(True)

  formatted
  |> string.contains("Author: John Doe")
  |> should.equal(True)
}

// Test 22: Format Research Article
pub fn format_research_article_test() {
  let article =
    news_and_research.ResearchArticle(
      request_id: 1001,
      article_id: "RES12345",
      title: "AAPL Buy Rating",
      author: "Analyst Team",
      pub_date: "2024-01-07 10:00:00",
      summary: "Strong earnings expected",
      rating: "BUY",
      target_price: 200.0,
    )

  let formatted = news_and_research.format_research_article(article)

  formatted
  |> string.contains("Article ID: RES12345")
  |> should.equal(True)

  formatted
  |> string.contains("Rating: BUY")
  |> should.equal(True)

  formatted
  |> string.contains("Target Price: 200.0")
  |> should.equal(True)
}

// Test 23: Format News Bulletin CSV
pub fn format_news_bulletin_csv_test() {
  let bulletin =
    news_and_research.NewsBulletin(
      msg_id: 1001,
      bulletin_type: news_and_research.GeneralNews,
      message: "Market rally",
      exchange: "NYSE",
    )

  let csv = news_and_research.format_news_bulletin_csv(bulletin)

  csv
  |> string.contains("1001")
  |> should.equal(True)

  csv
  |> string.contains("GENERAL NEWS")
  |> should.equal(True)

  csv
  |> string.contains("NYSE")
  |> should.equal(True)
}

// Test 24: Format News Article CSV
pub fn format_news_article_csv_test() {
  let article =
    news_and_research.NewsArticle(
      request_id: 1001,
      article_id: "ART12345",
      title: "Tech stocks rally",
      author: "John Doe",
      pub_date: "2024-01-07",
      summary: "Technology stocks surge",
      url: "https://example.com",
    )

  let csv = news_and_research.format_news_article_csv(article)

  csv
  |> string.contains("ART12345")
  |> should.equal(True)

  csv
  |> string.contains("Tech stocks rally")
  |> should.equal(True)

  csv
  |> string.contains("John Doe")
  |> should.equal(True)
}

// Test 25: Format Research Article CSV
pub fn format_research_article_csv_test() {
  let article =
    news_and_research.ResearchArticle(
      request_id: 1001,
      article_id: "RES12345",
      title: "AAPL Buy Rating",
      author: "Analyst Team",
      pub_date: "2024-01-07",
      summary: "Strong earnings",
      rating: "BUY",
      target_price: 200.0,
    )

  let csv = news_and_research.format_research_article_csv(article)

  csv
  |> string.contains("RES12345")
  |> should.equal(True)

  csv
  |> string.contains("BUY")
  |> should.equal(True)

  csv
  |> string.contains("200.0")
  |> should.equal(True)
}
