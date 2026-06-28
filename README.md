webmail-guess
=============

A lightweight Scala library to guess the webmail service URL by a given email address, reinforced with active DNS MX-records validation to filter out dead or non-existent domains.

## Usage

### 1. Initialization
Initialize the guesser with default servers configurations:

```scala
import com.hitsoft.guess.GuessWebmail

val guesser = GuessWebmail(GuessWebmail.servers.all)
```

### 2. Basic Query (Scala)
The .guess(email) method returns a GuessResult sealed trait, which has three distinct subtypes depending on the domain's state:

```scala
guesser.guess("user@gmail.com") match {
  case WithWebmail(url) =>
    println("Known webmail interface found: $url.")

  case NoWebmailHasMx =>
    println("Domain is valid and accepts emails, but the webmail interface URL is unknown.")

  case NoMxRecords =>
    println("Domain has no active MX records or does not exist.")
}
```

### 3. Java Compatibility
The library provides native boolean helpers inside GuessResult for convenient use in Java projects without dealing with Scala's companion object ($) syntax.

```Java
import com.hitsoft.guess.GuessWebmail.GuessResult;
import com.hitsoft.guess.GuessWebmail.WithWebmail;

GuessResult result = GuessWebmail(GuessWebmail.servers.all).guess("user@gmail.com");

if (result.isNoMxRecords()) {
    // Handle invalid domain
} else if (result.isWithWebmail()) {
    String webmailUrl = ((WithWebmail) result).url();
    // Use webmail url
} else if (result.isNoWebmailHasMx()) {
    // Valid corporate email
}
```
