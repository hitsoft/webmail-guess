package com.hitsoft.guess

import java.util.{Timer, TimerTask}
import javax.naming.NamingException
import javax.naming.directory.{Attribute, InitialDirContext}

import com.hitsoft.guess.GuessWebmail._

import scala.collection.mutable

/**
 * User: smeagol
 * Date: 20.12.14
 * Time: 3:17
 */

class GuessWebmail {

  private[guess] object dns {
    val serverDomains = mutable.Map.empty[String, mutable.Set[String]]
    val domainHosts = mutable.Map.empty[String, mutable.Set[String]]
    val hostIps = mutable.Map.empty[String, mutable.Set[String]]
  }

  private[guess] object lookupServerBy {
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
    server.domains.foreach(saveResolvedDomain(_, server.url))
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

  private[guess] def stripLastDot(value: String): String = {
    if (value.endsWith("."))
      value.substring(0, value.length - 1)
    else
      ""
  }

  private[guess] def resolveMx(domain: String): List[String] = resolveDNS(domain, "MX").map(row => {
    val parts = row.split(" ")
    if (parts.length == 1)
      parts(0)
    else
      parts(1)
  }).map(stripLastDot)

  private[guess] def resolveCname(host: String): List[String] = {
    var result = resolveDNS(host, "CNAME").map(stripLastDot)
    for (h <- result.toSeq) {
      result = result ++ resolveCname(h)
    }
    result.distinct
  }

  private[guess] def resolveIP(host: String): List[String] = resolveDNS(host, "A")

  private[guess] def resolveDNS(query: String, kind: String): List[String] = {
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

  private[guess] def resolveMxHosts(domain: String): List[String] = {
    var hosts = resolveMx(domain)
    for (host <- hosts.toSeq) {
      hosts = hosts ++ resolveCname(host)
    }
    hosts.distinct
  }

  private[guess] def refreshDomainDnsCache(domain: String): Unit = {
    val newHosts = resolveMxHosts(domain)
    val hosts = this.synchronized {
      dns.domainHosts.getOrElseUpdate(domain, mutable.Set.empty[String]) ++= newHosts
    }
    for (host <- hosts) {
      val ips = resolveIP(host)
      this.synchronized {
        dns.hostIps.getOrElseUpdate(host, mutable.Set.empty[String]) ++= ips
      }
    }
    this.synchronized {
      val urls = lookupServerBy.mx.filter(row => hosts.contains(row._1)).values.toSet
      if (urls.size == 1) {
        saveResolvedDomain(domain, urls.head)
      }
    }
  }

  /**
   * Check the list of specified webmails agains DNS to resolve it's mx records. This should be called at least once after all webmails specified or scheduled with {@link com.hitsoft.guess.GuessWebmail#schedule schedule} method.
   * @return GuessWebmail object for chaining
   */
  def refreshWebmailDnsCache(): GuessWebmail = {
    val domains = dns.serverDomains.values.flatten.toSet
    domains.foreach(refreshDomainDnsCache)
    invalidateCaches()
    this
  }

  private[guess] def saveResolvedDomain(domain: String, url: String): Unit = {
    lookupServerBy.domain.put(domain, url)
    val hosts = dns.domainHosts.getOrElse(domain, Set()).toSeq
    for ((host, ips) <- dns.hostIps.filterKeys(hosts.contains)) {
      lookupServerBy.mx.put(host, url)
      ips.foreach(lookupServerBy.ip.put(_, url))
    }
  }

  private[guess] def invalidateCaches(): Unit = {
    for ((url, domains) <- dns.serverDomains) {
      domains.foreach(domain => this.synchronized {
        saveResolvedDomain(domain, url)
      })
    }
    val domains = dns.domainHosts.keySet diff dns.serverDomains.values.flatten.toSet
    for (domain <- domains) {
      lookupServerByMx(domain).orElse(
        lookupServerByIp(domain)
      ) match {
        case Some(url) =>
          this.synchronized {
            saveResolvedDomain(domain, url)
          }
        case None =>
      }
    }
  }

  /**
   * Loads webmail dns cache from some external source
   * @param data webmail servers cache data loaded from some external storage, for example file or database
   * @return
   */
  def loadWebmailDnsCache(data: WebmailDnsCache) = this.synchronized {
    for ((dataUrl, dataDomains) <- data) {
      val domains = dns.serverDomains.getOrElseUpdate(dataUrl, mutable.Set.empty[String])
      for ((dataDomain, dataHosts) <- dataDomains) {
        domains.add(dataDomain)
        val hosts = dns.domainHosts.getOrElseUpdate(dataDomain, mutable.Set.empty[String])
        for ((dataHost, dataIps) <- dataHosts) {
          hosts.add(dataHost)
          dns.hostIps.getOrElseUpdate(dataHost, mutable.Set.empty[String]) ++= dataIps
        }
      }
    }
    invalidateCaches()
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
    for ((dataDomain, dataHosts) <- data) {
      val hosts = dns.domainHosts.getOrElseUpdate(dataDomain, mutable.Set.empty[String])
      for ((dataHost, dataIps) <- dataHosts) {
        hosts.add(dataHost)
        dns.hostIps.getOrElseUpdate(dataHost, mutable.Set.empty[String]) ++= dataIps
      }
    }
    invalidateCaches()
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

  private[guess] class RefreshDnsTask extends TimerTask {
    def run() {
      try {
        refreshWebmailDnsCache()
      } catch {
        case e: Exception =>
      }
    }
  }

  /**
   * Schedule timer for periodical webmails DNS refresh with {@link com.hitsoft.guess.GuessWebmail#refreshWebmailDnsCache refreshWebmailDnsCache} method.
   * @param delay delay before first run in milliseconds
   * @param period period of periodical refresh in milliseconds
   * @return Timer object
   */
  def schedule(delay: Long, period: Long): Timer = {
    val timer = new Timer
    timer.schedule(new RefreshDnsTask(), delay, period)
    timer
  }

  private[guess] def extractDomain(email: String) = {
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
    val domainHosts = dns.domainHosts.getOrElse(domain, Set[String]())
    domainHosts.intersect(lookupServerBy.mx.keySet).headOption match {
      case None => None
      case Some(host) => lookupServerBy.mx.get(host)
    }
  }

  def lookupServerByIp(domain: String): Option[String] = this.synchronized {
    val domainHosts = dns.domainHosts.getOrElse(domain, Set[String]())
    val hostsIps = dns.hostIps.filterKeys(domainHosts.contains).values.flatten
    hostsIps.toSet.intersect(lookupServerBy.ip.keySet).headOption match {
      case None => None
      case Some(ip) => lookupServerBy.ip.get(ip)
    }
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
