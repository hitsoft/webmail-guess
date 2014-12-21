package com.hitsoft.guess

import com.hitsoft.guess.GuessWebmail.Server
import org.specs2._
import scala.collection.mutable

/**
 * User: smeagol
 * Date: 21.12.14
 * Time: 0:32
 */
class GuessWebmailTest extends Specification {
  def is = s2"""
    1. Add servers
        add one ${Add().one}
        add some ${Add().some}
        add separate with one domain ${Add().separateOne}
        add separate with multiple domains ${Add().separateSome}

    2. Load and save caches for some permanent storage
        load webmail dns cache ${LoadSave().loadWebmailDnsCache}
        load domains dns cache ${LoadSave().loadDomainDnsCache}
        load webmail dns cache after domains ${LoadSave().loadWebmailDnsCacheAfterDomainCache}
        load domains dns cache after webmail ${LoadSave().loadDomainDnsCacheAfterWebmailCache}
        save webmail dns cache ${LoadSave().saveWebmailDnsCache}
        save domains dns cache ${LoadSave().saveDomainDnsCache}

    3. Lookup
        lookup by domain hit ${Lookup().byDomain}
        lookup by mx hit ${Lookup().byMx}
        lookup by ip hit ${Lookup().byIp}

    4. Resolve DNS records
        resolve ip address ${Resolve().ip}
        resolve cname address ${Resolve().cname}
        resolve mx address ${Resolve().mx}
    """

  case class Add() {
    def one = {
      val guess = new GuessWebmail()
      guess.add(Server("url", Seq("domain")))
      (guess.dns.serverDomains should_== Map("url" -> Set("domain"))) and
        (guess.dns.domainHosts should_== Map()) and
        (guess.dns.hostIps should_== Map()) and
        (guess.lookupServerBy.domain should_== Map("domain" -> "url")) and
        (guess.lookupServerBy.mx should_== Map()) and
        (guess.lookupServerBy.ip should_== Map())
    }

    def some = {
      val guess = new GuessWebmail()
      guess.add(Seq(Server("url1", Seq("domain1")), Server("url2", Seq("domain2"))))
      (guess.dns.serverDomains should_== Map(
        "url1" -> Set("domain1"),
        "url2" -> Set("domain2")
      )) and
        (guess.dns.domainHosts should_== Map()) and
        (guess.dns.hostIps should_== Map()) and
        (guess.lookupServerBy.domain should_== Map(
          "domain1" -> "url1",
          "domain2" -> "url2"
        )) and
        (guess.lookupServerBy.mx should_== Map()) and
        (guess.lookupServerBy.ip should_== Map())
    }

    def separateOne = {
      val guess = new GuessWebmail()
      guess.add("url", "domain")
      (guess.dns.serverDomains should_== Map("url" -> Set("domain"))) and
        (guess.dns.domainHosts should_== Map()) and
        (guess.dns.hostIps should_== Map()) and
        (guess.lookupServerBy.domain should_== Map("domain" -> "url")) and
        (guess.lookupServerBy.mx should_== Map()) and
        (guess.lookupServerBy.ip should_== Map())
    }

    def separateSome = {
      val guess = new GuessWebmail()
      guess.add("url", Seq("domain1", "domain2"))
      (guess.dns.serverDomains should_== Map("url" -> Set("domain1", "domain2"))) and
        (guess.dns.domainHosts should_== Map()) and
        (guess.dns.hostIps should_== Map()) and
        (guess.lookupServerBy.domain should_== Map(
          "domain1" -> "url",
          "domain2" -> "url"
        )) and
        (guess.lookupServerBy.mx should_== Map()) and
        (guess.lookupServerBy.ip should_== Map())
    }
  }

  case class LoadSave() {
    def loadWebmailDnsCache = {
      val guess = new GuessWebmail()
      guess.loadWebmailDnsCache(Map(
        "url" -> Map(
          "domain" -> Map(
            "host" -> Seq("ip")
          )
        )
      ))
      (guess.dns.serverDomains should_== Map(
        "url" -> Set("domain")
      )) and
        (guess.dns.domainHosts should_== Map(
          "domain" -> Set("host")
        )) and
        (guess.dns.hostIps should_== Map(
          "host" -> Set("ip")
        )) and
        (guess.lookupServerBy.domain should_== Map(
          "domain" -> "url"
        )) and
        (guess.lookupServerBy.mx should_== Map(
          "host" -> "url"
        )) and
        (guess.lookupServerBy.ip should_== Map(
          "ip" -> "url"
        ))
    }

    def loadDomainDnsCache = {
      val guess = new GuessWebmail()
      guess.loadDomainDnsCache(Map(
        "domain" -> Map(
          "host" -> Seq("ip")
        )
      ))
      (guess.dns.serverDomains should_== Map()) and
        (guess.dns.domainHosts should_== Map(
          "domain" -> Set("host")
        )) and
        (guess.dns.hostIps should_== Map(
          "host" -> Set("ip")
        )) and
        (guess.lookupServerBy.domain should_== Map()) and
        (guess.lookupServerBy.mx should_== Map()) and
        (guess.lookupServerBy.ip should_== Map())
    }

    def loadDomainDnsCacheAfterWebmailCache = {
      val guess = new GuessWebmail()
      guess.loadWebmailDnsCache(Map(
        "url.w" -> Map(
          "domain.w" -> Map(
            "host.w" -> Seq("ip.w")
          )
        )
      ))
      guess.loadDomainDnsCache(Map(
        "domain.1.n" -> Map(
          "host.1.n" -> Seq("ip.1.n")
        ),
        "domain.w" -> Map(
          "host.2.w" -> Seq("ip.2.w")
        ),
        "domain.3.w" -> Map(
          "host.w" -> Seq("ip.3.w")
        ),
        "domain.4.w" -> Map(
          "host.4.w" -> Seq("ip.w")
        )
      ))
      (guess.dns.serverDomains should_== Map(
        "url.w" -> Set("domain.w")
      )) and
        (guess.dns.domainHosts should_== Map(
          "domain.w" -> Set("host.w", "host.2.w"),
          "domain.1.n" -> Set("host.1.n"),
          "domain.3.w" -> Set("host.w"),
          "domain.4.w" -> Set("host.4.w")
        )) and
        (guess.dns.hostIps should_== Map(
          "host.w" -> Set("ip.w", "ip.3.w"),
          "host.1.n" -> Set("ip.1.n"),
          "host.2.w" -> Set("ip.2.w"),
          "host.4.w" -> Set("ip.w")
        )) and
        (guess.lookupServerBy.domain should_== Map(
          "domain.w" -> "url.w",
          "domain.3.w" -> "url.w",
          "domain.4.w" -> "url.w"
        )) and
        (guess.lookupServerBy.mx should_== Map(
          "host.w" -> "url.w",
          "host.2.w" -> "url.w",
          "host.4.w" -> "url.w"
        )) and
        (guess.lookupServerBy.ip should_== Map(
          "ip.w" -> "url.w",
          "ip.2.w" -> "url.w",
          "ip.3.w" -> "url.w"
        ))
    }

    def loadWebmailDnsCacheAfterDomainCache = {
      val guess = new GuessWebmail()
      guess.loadDomainDnsCache(Map(
        "domain.1.n" -> Map(
          "host.1.n" -> Seq("ip.1.n")
        ),
        "domain.w" -> Map(
          "host.2.w" -> Seq("ip.2.w")
        ),
        "domain.3.w" -> Map(
          "host.w" -> Seq("ip.3.w")
        ),
        "domain.4.w" -> Map(
          "host.4.w" -> Seq("ip.w")
        )
      ))
      guess.loadWebmailDnsCache(Map(
        "url.w" -> Map(
          "domain.w" -> Map(
            "host.w" -> Seq("ip.w")
          )
        )
      ))
      (guess.dns.serverDomains should_== Map(
        "url.w" -> Set("domain.w")
      )) and
        (guess.dns.domainHosts should_== Map(
          "domain.w" -> Set("host.w", "host.2.w"),
          "domain.1.n" -> Set("host.1.n"),
          "domain.3.w" -> Set("host.w"),
          "domain.4.w" -> Set("host.4.w")
        )) and
        (guess.dns.hostIps should_== Map(
          "host.w" -> Set("ip.w", "ip.3.w"),
          "host.1.n" -> Set("ip.1.n"),
          "host.2.w" -> Set("ip.2.w"),
          "host.4.w" -> Set("ip.w")
        )) and
        (guess.lookupServerBy.domain should_== Map(
          "domain.w" -> "url.w",
          "domain.3.w" -> "url.w",
          "domain.4.w" -> "url.w"
        )) and
        (guess.lookupServerBy.mx should_== Map(
          "host.w" -> "url.w",
          "host.2.w" -> "url.w",
          "host.4.w" -> "url.w"
        )) and
        (guess.lookupServerBy.ip should_== Map(
          "ip.w" -> "url.w",
          "ip.2.w" -> "url.w",
          "ip.3.w" -> "url.w"
        ))
    }

    def saveWebmailDnsCache = {
      val guess = new GuessWebmail()
      guess.dns.serverDomains.put("url", mutable.Set("domain.1.w", "domain.2.w"))
      guess.dns.domainHosts.put("domain.1.w", mutable.Set("host.1.w"))
      guess.dns.domainHosts.put("domain.2.w", mutable.Set("host.2.w"))
      guess.dns.domainHosts.put("domain.3.n", mutable.Set("host.3.n"))
      guess.dns.hostIps.put("host.1.w", mutable.Set("ip.1.w"))
      guess.dns.hostIps.put("host.2.w", mutable.Set("ip.2.w"))
      guess.dns.hostIps.put("host.3.n", mutable.Set("ip.3.n"))
      guess.saveWebmailDnsCache should_== Map(
        "url" -> Map(
          "domain.1.w" -> Map(
            "host.1.w" -> Seq("ip.1.w")
          ),
          "domain.2.w" -> Map(
            "host.2.w" -> Seq("ip.2.w")
          )
        )
      )
    }

    def saveDomainDnsCache = {
      val guess = new GuessWebmail()
      guess.dns.serverDomains.put("url", mutable.Set("domain.1.w", "domain.2.w"))
      guess.dns.domainHosts.put("domain.1.w", mutable.Set("host.1.w"))
      guess.dns.domainHosts.put("domain.2.w", mutable.Set("host.2.w"))
      guess.dns.domainHosts.put("domain.3.n", mutable.Set("host.3.n"))
      guess.dns.hostIps.put("host.1.w", mutable.Set("ip.1.w"))
      guess.dns.hostIps.put("host.2.w", mutable.Set("ip.2.w"))
      guess.dns.hostIps.put("host.3.n", mutable.Set("ip.3.n"))
      guess.saveDomainDnsCache should_== Map(
        "domain.3.n" -> Map(
          "host.3.n" -> Seq("ip.3.n")
        )
      )
    }
  }

  case class Lookup() {
    def byDomain = {
      val guess = new GuessWebmail()
      guess.loadWebmailDnsCache(Map(
        "url" -> Map(
          "domain" -> Map(
            "host" -> Seq("ip")
          )
        )
      ))
      (guess.lookupServerByDomain("domain") should_== Some("url")) and
        (guess.lookupServerByDomain("test") should_== None)
    }

    def byMx = {
      val guess = new GuessWebmail()
      guess.loadWebmailDnsCache(Map(
        "url" -> Map(
          "domain" -> Map(
            "host" -> Seq("ip")
          )
        )
      ))
      guess.loadDomainDnsCache(Map(
        "test1" -> Map(
          "host" -> Seq("ip1")
        ),
        "test2" -> Map(
          "host2" -> Seq("ip2")
        )
      ))
      (guess.lookupServerByMx("test1") should_== Some("url")) and
        (guess.lookupServerByMx("test2") should_== None)
    }

    def byIp = {
      val guess = new GuessWebmail()
      guess.loadWebmailDnsCache(Map(
        "url" -> Map(
          "domain" -> Map(
            "host" -> Seq("ip")
          )
        )
      ))
      guess.loadDomainDnsCache(Map(
        "test1" -> Map(
          "host1" -> Seq("ip")
        ),
        "test2" -> Map(
          "host2" -> Seq("ip2")
        )
      ))
      (guess.lookupServerByMx("test1") should_== Some("url")) and
        (guess.lookupServerByMx("test2") should_== None)
    }
  }

  case class Resolve() {
    def ip = {
      val guess = new GuessWebmail()
      guess.resolveIP("litera5.ru") should_== Seq("5.9.205.165")
    }
    def cname = {
      val guess = new GuessWebmail()
      guess.resolveCname("mail.litera5.ru") should_== Seq("mx.yandex.ru")
    }
    def mx = {
      val guess = new GuessWebmail()
      guess.resolveMx("litera5.ru") should_== Seq("mx.yandex.ru")
    }
  }
}