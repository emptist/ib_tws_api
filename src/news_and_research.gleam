import gleam/bit_array
import gleam/float
import gleam/int
import gleam/list
import gleam/string

/// News and Research Module
/// 
/// This module provides functionality to receive news bulletins,
/// research articles, and market news updates.
/// Staying informed about market events is crucial for making
/// informed trading decisions.
/// News bulletin type
pub type NewsBulletinType {
  GeneralNews
  Regulatory
  CorporateAction
  MarketEvent
  AnalystComment
}

/// News bulletin
pub type NewsBulletin {
  NewsBulletin(
    /// Message ID
    msg_id: Int,
    /// Bulletin type
    bulletin_type: NewsBulletinType,
    /// Message content
    message: String,
    /// Exchange
    exchange: String,
  )
}

/// News article
pub type NewsArticle {
  NewsArticle(
    /// Request ID
    request_id: Int,
    /// Article ID
    article_id: String,
    /// Title
    title: String,
    /// Author
    author: String,
    /// Publication date
    pub_date: String,
    /// Summary
    summary: String,
    /// URL
    url: String,
  )
}

/// Research article
pub type ResearchArticle {
  ResearchArticle(
    /// Request ID
    request_id: Int,
    /// Article ID
    article_id: String,
    /// Title
    title: String,
    /// Author
    author: String,
    /// Publication date
    pub_date: String,
    /// Summary
    summary: String,
    /// Rating
    rating: String,
    /// Target price
    target_price: Float,
  )
}

/// Request news bulletins
pub fn request_news_bulletins(all_messages: Bool) -> BitArray {
  // Message format for REQ_NEWS_BULLETINS (MsgCode 11)
  let all_msg_flag = case all_messages {
    True -> 1
    False -> 0
  }
  <<11:size(8), all_msg_flag:size(8)>>
}

/// Cancel news bulletins
pub fn cancel_news_bulletins() -> BitArray {
  // Message format for CANCEL_NEWS_BULLETINS (MsgCode 12)
  <<12:size(8)>>
}

/// Request news article
pub fn request_news_article(request_id: Int, article_id: String) -> BitArray {
  // Message format for REQ_NEWS_ARTICLE (MsgCode 13)
  // Simplified - actual implementation would encode the article ID
  bit_array.concat([<<13:size(8)>>, int_to_field(request_id)])
}

/// Request historical news
pub fn request_historical_news(
  request_id: Int,
  contract_id: Int,
  provider_codes: String,
  start_time: String,
  end_time: String,
  total_results: Int,
) -> BitArray {
  // Message format for REQ_HISTORICAL_NEWS (MsgCode 14)
  bit_array.concat([
    <<14:size(8)>>,
    int_to_field(request_id),
    int_to_field(contract_id),
    string_to_field(provider_codes),
    string_to_field(start_time),
    string_to_field(end_time),
    int_to_field(total_results),
  ])
}

/// Cancel historical news
pub fn cancel_historical_news(request_id: Int) -> BitArray {
  // Message format for CANCEL_HISTORICAL_NEWS (MsgCode 15)
  bit_array.concat([<<15:size(8)>>, int_to_field(request_id)])
}

/// Request research articles
pub fn request_research_articles(
  request_id: Int,
  contract_id: Int,
  provider_code: String,
) -> BitArray {
  // Message format for REQ_RESEARCH_ARTICLES (MsgCode 16)
  bit_array.concat([
    <<16:size(8)>>,
    int_to_field(request_id),
    int_to_field(contract_id),
    string_to_field(provider_code),
  ])
}

/// Cancel research articles
pub fn cancel_research_articles(request_id: Int) -> BitArray {
  // Message format for CANCEL_RESEARCH_ARTICLES (MsgCode 17)
  bit_array.concat([<<17:size(8)>>, int_to_field(request_id)])
}

/// Analysis Functions
/// Filter news bulletins by type
pub fn filter_bulletins_by_type(
  bulletins: List(NewsBulletin),
  bulletin_type: NewsBulletinType,
) -> List(NewsBulletin) {
  list.filter(bulletins, fn(bulletin) {
    bulletin.bulletin_type == bulletin_type
  })
}

/// Filter news bulletins by exchange
pub fn filter_bulletins_by_exchange(
  bulletins: List(NewsBulletin),
  exchange: String,
) -> List(NewsBulletin) {
  list.filter(bulletins, fn(bulletin) { bulletin.exchange == exchange })
}

/// Get news bulletins containing specific keywords
pub fn search_bulletins_by_keyword(
  bulletins: List(NewsBulletin),
  keyword: String,
) -> List(NewsBulletin) {
  list.filter(bulletins, fn(bulletin) {
    string.contains(bulletin.message, keyword)
  })
}

/// Get news articles by author
pub fn get_articles_by_author(
  articles: List(NewsArticle),
  author: String,
) -> List(NewsArticle) {
  list.filter(articles, fn(article) { article.author == author })
}

/// Get research articles with specific rating
pub fn get_research_by_rating(
  articles: List(ResearchArticle),
  rating: String,
) -> List(ResearchArticle) {
  list.filter(articles, fn(article) { article.rating == rating })
}

/// Get research articles with target price above threshold
pub fn get_research_by_target_price(
  articles: List(ResearchArticle),
  min_price: Float,
) -> List(ResearchArticle) {
  list.filter(articles, fn(article) { article.target_price >=. min_price })
}

/// Get research articles with target price below threshold
pub fn get_research_below_target_price(
  articles: List(ResearchArticle),
  max_price: Float,
) -> List(ResearchArticle) {
  list.filter(articles, fn(article) { article.target_price <=. max_price })
}

/// Count news bulletins by type
pub fn count_bulletins_by_type(
  bulletins: List(NewsBulletin),
  bulletin_type: NewsBulletinType,
) -> Int {
  bulletins
  |> filter_bulletins_by_type(bulletin_type)
  |> list.length()
}

/// Formatting Functions
/// Format news bulletin type for display
pub fn format_bulletin_type(bulletin_type: NewsBulletinType) -> String {
  case bulletin_type {
    GeneralNews -> "GENERAL NEWS"
    Regulatory -> "REGULATORY"
    CorporateAction -> "CORPORATE ACTION"
    MarketEvent -> "MARKET EVENT"
    AnalystComment -> "ANALYST COMMENT"
  }
}

/// Format news bulletin for display
pub fn format_news_bulletin(bulletin: NewsBulletin) -> String {
  let type_str = format_bulletin_type(bulletin.bulletin_type)

  "
News Bulletin
  Message ID: " <> int.to_string(bulletin.msg_id) <> "
  Type: " <> type_str <> "
  Exchange: " <> bulletin.exchange <> "
  Message: " <> bulletin.message
}

/// Format news article for display
pub fn format_news_article(article: NewsArticle) -> String {
  "
News Article
  Article ID: " <> article.article_id <> "
  Title: " <> article.title <> "
  Author: " <> article.author <> "
  Date: " <> article.pub_date <> "
  Summary: " <> article.summary <> "
  URL: " <> article.url
}

/// Format research article for display
pub fn format_research_article(article: ResearchArticle) -> String {
  "
Research Article
  Article ID: " <> article.article_id <> "
  Title: " <> article.title <> "
  Author: " <> article.author <> "
  Date: " <> article.pub_date <> "
  Rating: " <> article.rating <> "
  Target Price: " <> float.to_string(article.target_price) <> "
  Summary: " <> article.summary
}

/// Format news bulletin as CSV
pub fn format_news_bulletin_csv(bulletin: NewsBulletin) -> String {
  let type_str = format_bulletin_type(bulletin.bulletin_type)

  int.to_string(bulletin.msg_id)
  <> ","
  <> type_str
  <> ","
  <> bulletin.exchange
  <> ","
  <> bulletin.message
}

/// Format news article as CSV
pub fn format_news_article_csv(article: NewsArticle) -> String {
  article.article_id
  <> ","
  <> article.title
  <> ","
  <> article.author
  <> ","
  <> article.pub_date
  <> ","
  <> article.summary
  <> ","
  <> article.url
}

/// Format research article as CSV
pub fn format_research_article_csv(article: ResearchArticle) -> String {
  article.article_id
  <> ","
  <> article.title
  <> ","
  <> article.author
  <> ","
  <> article.pub_date
  <> ","
  <> article.rating
  <> ","
  <> float.to_string(article.target_price)
  <> ","
  <> article.summary
}

/// Helper Functions
/// Convert integer to field (simplified)
fn int_to_field(value: Int) -> BitArray {
  // Simplified implementation
  bit_array.from_string(int.to_string(value) <> "\\0")
}

/// Convert string to field (simplified)
fn string_to_field(value: String) -> BitArray {
  // Simplified implementation
  bit_array.from_string(value <> "\\0")
}

/// Parse news bulletin (placeholder - to be implemented in message_handler)
pub fn parse_news_bulletin(data: BitArray) -> Result(NewsBulletin, String) {
  // This will be implemented when parsing actual news data
  Error("Not yet implemented - will be added to message_handler")
}

/// Parse news article (placeholder - to be implemented in message_handler)
pub fn parse_news_article(data: BitArray) -> Result(NewsArticle, String) {
  // This will be implemented when parsing actual news data
  Error("Not yet implemented - will be added to message_handler")
}

/// Parse research article (placeholder - to be implemented in message_handler)
pub fn parse_research_article(data: BitArray) -> Result(ResearchArticle, String) {
  // This will be implemented when parsing actual research data
  Error("Not yet implemented - will be added to message_handler")
}
