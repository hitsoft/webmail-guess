package com.hitsoft.guess

import java.util
import java.util.{TimerTask, Timer}
import javax.naming.NamingException
import javax.naming.directory.{Attribute, InitialDirContext}

import com.hitsoft.guess.GuessWebmail._
import org.joda.time.DateTimeConstants

import scala.collection.mutable

import com.github.nscala_time.time.Imports._

/**
 * User: smeagol
 * Date: 20.12.14
 * Time: 3:17
 */

class GuessWebmail {

  private class ExpireDnsTask extends TimerTask {
    def run() {
      try {
        this.synchronized {
          val expired = dns.expire.filter(item => item._1.isBeforeNow)
          expired.keys.foreach(dns.expire.remove)
          for (domain <- expired.values.flatten) {
            dns.domainHosts.getOrElse(domain, Set()).foreach(dns.hostIps.remove)
            dns.domainHosts.remove(domain)
          }
        }
      } catch {
        case e: Exception =>
      }
    }
  }

  private val expireTimer = {
    val timer = new Timer
    timer.schedule(new ExpireDnsTask(), 0, DateTimeConstants.MILLIS_PER_MINUTE)
    timer
  }

  private object dns {
    val expire = mutable.Map.empty[DateTime, mutable.Set[String]]

    val serverDomains = mutable.Map.empty[String, mutable.Set[String]]
    val domainHosts = mutable.Map.empty[String, mutable.Set[String]]
    val hostIps = mutable.Map.empty[String, mutable.Set[String]]
  }

  private object lookupServerBy {
    val domain = mutable.Map.empty[String, String]
    val mx = mutable.Map.empty[String, String]
    val ip = mutable.Map.empty[String, String]
  }

  /**
   * Adds some webmail server to the list of available webmails
   * @param server webmail server to add
   * @return GuessWebmail object for chaining
   */
  def add(server: Server): GuessWebmail = this.synchronized {
    dns.serverDomains.getOrElseUpdate(server.url.toLowerCase, mutable.Set.empty[String]) ++= server.domains
    this
  }

  /**
   * Adds some webmail servers to the list of available webmails
   * @param servers list of webmail servers to add
   * @return GuessWebmail object for chaining
   */
  def add(servers: Seq[Server]): GuessWebmail = {
    servers.foreach(add)
    this
  }

  /**
   * Adds some webmail server to the list of available webmails
   * @param url url for new webmail server
   * @param domains list of email domains the server serves
   * @return GuessWebmail object for chaining
   */
  def add(url: String, domains: Seq[String]): GuessWebmail = add(Server(url, domains))

  /**
   * Adds some webmail server to the list of available webmails
   * @param url url for new webmail server
   * @param domain email domain the server serves
   * @return GuessWebmail object for chaining
   */
  def add(url: String, domain: String): GuessWebmail = add(url, Seq(domain))

  private def stripLastDot(value: String): String = {
    if (value.endsWith("."))
      value.substring(0, value.length - 1)
    else
      ""
  }

  private def resolveMx(domain: String): List[String] = lookupDns(domain, "MX").map(row => {
    val parts = row.split(" ")
    if (parts.length == 1)
      parts(0)
    else
      parts(1)
  }).map(stripLastDot)

  private def resolveCname(host: String): List[String] = {
    var result = lookupDns(host, "CNAME").map(stripLastDot)
    for (h <- result.toSeq) {
      result = result ++ resolveCname(h)
    }
    result.distinct
  }

  private def resolveIP(host: String): List[String] = lookupDns(host, "A")

  private def lookupDns(query: String, kind: String): List[String] = {
    val attributes = try {
      new InitialDirContext().getAttributes(s"dns:/$query", Array(kind))
    } catch {
      case _: NamingException => return Nil
    }
    val list = {
      val attributeEnumeration = attributes.getAll
      var list = List[Attribute]()
      while (attributeEnumeration.hasMore)
        list = attributeEnumeration.next :: list
      attributeEnumeration.close()
      list.reverse
    }
    list map (x => x.get.toString)
  }

  private def resolveMxHosts(domain: String): List[String] = {
    var hosts = resolveMx(domain)
    for (host <- hosts.toSeq) {
      hosts = hosts ++ resolveCname(host)
    }
    hosts.distinct
  }

  private def refreshDomainDnsCache(domain: String): Unit = {
    if (!dns.domainHosts.contains(domain)) {
      val newHosts = resolveMxHosts(domain)
      val hosts = this.synchronized {
        dns.domainHosts.getOrElse(domain, mutable.Set.empty[String]) ++= newHosts
      }
      for (host <- hosts) {
        val ips = resolveIP(host)
        this.synchronized {
          dns.hostIps.getOrElse(host, mutable.Set.empty[String]) ++= ips
        }
      }
      this.synchronized {
        val urls = lookupServerBy.mx.filter(row => hosts.contains(row._1)).values.toSet
        if (urls.size == 1) {
          val url = urls.head
          lookupServerBy.domain.put(domain, url)
          hosts.foreach(lookupServerBy.mx.put(_, url))
          val ips = dns.hostIps.filter(row => hosts.contains(row._1)).values.flatten.toSet
          ips.foreach(lookupServerBy.ip.put(_, url))
        }
      }
    }
  }

  /**
   * Check the list of specified webmails agains DNS to resolve it's mx records. This should be called at least once after all webmails specified or scheduled with {@link com.hitsoft.webmail_guess.WebmailGuess#schedule schedule} method.
   * @return GuessWebmail object for chaining
   */
  def refreshWebmailDnsCache(): GuessWebmail = {
    val domains = dns.serverDomains.values.flatten.toSet diff dns.domainHosts.keySet
    domains.foreach(refreshDomainDnsCache)
    this
  }

  /**
   * Loads webmail dns cache from some external source
   * @param data webmail servers cache data loaded from some external storage, for example file or database
   * @return
   */
  def loadWebmailDnsCache(data: WebmailDnsCache) = this.synchronized {
    val expires = dns.expire.getOrElseUpdate(DateTime.now.plusMinutes(10), mutable.Set.empty[String])
    for (server <- data) {
      val domains = dns.serverDomains.getOrElse(server._1, mutable.Set.empty[String])
      for (domain <- server._2) {
        domains.add(domain._1)
        expires.add(domain._1)
        val hosts = dns.domainHosts.getOrElse(domain._1, mutable.Set.empty[String])
        for (host <- domain._2) {
          hosts.add(host._1)
          dns.hostIps.getOrElse(host._1, mutable.Set.empty[String]) ++= host._2
        }
      }
    }
    this
  }

  /**
   * Saves current webmail dns cache to independent models for saving in some external storage
   * @return
   */
  def saveWebmailDnsCache: WebmailDnsCache = this.synchronized {
    dns.serverDomains.map(server =>
      server._1 -> server._2.map(domain =>
        domain -> dns.domainHosts.getOrElse(domain, Set()).map(host =>
          host -> dns.hostIps.getOrElse(host, Set()).toSeq
        ).toMap[String, Seq[String]]
      ).toMap[String, HostCache]
    ).toMap[String, Map[String, HostCache]]
  }

  /**
   * Loads domains DNS cache from some external source like file of database
   * @param data cache data to load
   * @return
   */
  def loadDomainDnsCache(data: DomainDnsCache) = this.synchronized {
    val expires = dns.expire.getOrElseUpdate(DateTime.now.plusMinutes(10), mutable.Set.empty[String])
    for (domain <- data) {
      expires.add(domain._1)
      val hosts = dns.domainHosts.getOrElse(domain._1, mutable.Set.empty[String])
      for (host <- domain._2) {
        hosts.add(host._1)
        dns.hostIps.getOrElse(host._1, mutable.Set.empty[String]) ++= host._2
      }
    }
    this
  }

  def saveDomainDnsCache: DomainDnsCache = this.synchronized {
    val domains = dns.domainHosts.keys.toSet diff dns.serverDomains.values.flatten.toSet
    domains.map(domain =>
      domain -> dns.domainHosts.getOrElse(domain, Set()).map(host =>
        host -> dns.hostIps.getOrElse(host, Set()).toSeq
      ).toMap[String, Seq[String]]
    ).toMap[String, HostCache]
  }

  private class RefreshDnsTask extends TimerTask {
    def run() {
      try {
        refreshWebmailDnsCache()
      } catch {
        case e: Exception =>
      }
    }
  }

  /**
   * Schedule timer for periodical webmails DNS refresh with {@link com.hitsoft.webmail_guess.WebmailGuess#refreshWebmailDnsCache refreshWebmailDnsCache} method.
   * @param delay delay before first run in milliseconds
   * @param period period of periodical refresh in milliseconds
   * @return Timer object
   */
  def schedule(delay: Long, period: Long): Timer = {
    val timer = new Timer
    timer.schedule(new RefreshDnsTask(), delay, period)
    timer
  }

  private def extractDomain(email: String) = {
    val parts = email.split("@")
    if (parts.length == 2)
      parts(1)
    else
      email
  }

  def lookupServerByDomain(domain: String): Option[String] = this.synchronized {
    lookupServerBy.domain.get(domain)
  }

  def lookupServerByMx(domain: String): Option[String] = this.synchronized {
    val domainHosts = dns.domainHosts.getOrElse(domain, mutable.Set.empty[String])
    domainHosts.intersect(lookupServerBy.mx.keySet).headOption
  }

  def lookupServerByIp(domain: String): Option[String] = this.synchronized {
    val domainHosts = dns.domainHosts.getOrElse(domain, Set[String]())
    val hostsIps = dns.hostIps.filterKeys(domainHosts.contains).values.flatten
    hostsIps.toSet.intersect(lookupServerBy.ip.keySet).headOption
  }

  /**
   * Check the specified email address against of configured webmails and returns the url of it's webmail if found something useful. Returns None if nothing matches
   * @param email email address or domain itself to guess it's webmail url
   * @return url of webmail service or None
   */
  def guess(email: String): Option[String] = {
    val domain = extractDomain(email)
    lookupServerByDomain(domain) match {
      case Some(result) => Some(result)
      case None =>
        refreshDomainDnsCache(domain)
        lookupServerByMx(domain).orElse(
          lookupServerByIp(domain)
        )
    }
  }
}

object GuessWebmail {

  /**
   * Map of Host names to list of it's IP addresses
   * @example
   * Map(
   * "mx1.gmail.com" -> Seq("192.168.192.1", "192.168.192.2"),
   * "mx2.gmail.com" -> Seq("192.168.192.3", "192.168.192.4")
   * )
   */
  type HostCache = Map[String, Seq[String]]
  /**
   * Map of Server url to domains it serves, that maps to the map of mx hosts to it's IP addresses
   * @example
   * Map(
   * "https://gmail.com" -> Map(
   * "gmail.com" -> Map(
   * "mx1.gmail.com" -> Seq("192.168.192.1", "192.168.192.2"),
   * "mx2.gmail.com" -> Seq("192.168.192.3", "192.168.192.4")
   * )
   * ),
   * "https://mail.ru" -> Map(
   * "mail.ru" -> Map(
   * "mx1.mail.ru" -> Seq("192.168.192.1", "192.168.192.2"),
   * "mx2.mail.ru" -> Seq("192.168.192.3", "192.168.192.4")
   * ),
   * "inbox.ru" -> Map(
   * "mx1.mail.ru" -> Seq("192.168.192.1", "192.168.192.2"),
   * "mx2.mail.ru" -> Seq("192.168.192.3", "192.168.192.4")
   * )
   * )
   * )
   */
  type WebmailDnsCache = Map[String, Map[String, HostCache]]
  /**
   * Map of domain to it's mx hosts to it's IP addresses
   * @example
   * Map(
   * "mail.ru" -> Map(
   * "mx1.mail.ru" -> Seq("192.168.192.1", "192.168.192.2"),
   * "mx2.mail.ru" -> Seq("192.168.192.3", "192.168.192.4")
   * ),
   * "inbox.ru" -> Map(
   * "mx1.mail.ru" -> Seq("192.168.192.1", "192.168.192.2"),
   * "mx2.mail.ru" -> Seq("192.168.192.3", "192.168.192.4")
   * )
   * )
   */
  type DomainDnsCache = Map[String, HostCache]

  case class Server(url: String, domains: Seq[String])

  object servers {
    val aol = Server("https://mail.aol.com", Seq("aol.com"))
    val fastMail = Server("https://fastmail.com", Seq("fastmail.com"))
    val gmail = Server("https://gmail.com", Seq("gmail.com"))
    val gmx = Server("https://gmx.com", Seq("gmx.com"))
    val inbox = Server("https://inbox.com", Seq("inbox.com"))
    val hotmail = Server("https://hotmail.com", Seq("hotmail.com"))
    val yahoo = Server("https://mail.yahoo.com", Seq("yahoo.com", "yahoo.com.ua", "yahoo.com.vn"))
    val mailRu = Server("https://mail.ru", Seq("mail.ru", "inbox.ru", "list.ru", "bk.ru", "mail.ua"))
    val yandex = Server("https://mail.yandex.ru", Seq("yandex.ru", "ya.ru", "narod.ru", "yandex.com", "yandex.kz", "yandex.by", "yandex.ua"))
    val rambler = Server("https://mail.rambler.ru", Seq("rambler.ru"))
    val ngs = Server("https://mail.ngs.ru", Seq("ngs.ru"))

    def all = Seq(aol, fastMail, gmail, gmx, inbox, hotmail, yahoo, mailRu, yandex, rambler, ngs)
  }

  def apply(servers: Seq[Server]) = new GuessWebmail().add(servers)

  def apply(server: Server) = new GuessWebmail().add(server)

  def apply(url: String, domains: Seq[String]) = new GuessWebmail().add(url, domains)

  def apply(url: String, domain: String) = new GuessWebmail().add(url, domain)

}
