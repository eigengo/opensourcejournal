Extending Spring Integration
----------------------------

## Abstract

Spring Integration (SI) offers numerous adapters out of the box covering the most common integration needs from web services to messaging systems with databases aplenty inbetween. What happens though when a requirement falls outside of the default offering?  Beans I hear you say?!  True enough, one can easily write new components to handle such scenarios. Say, you could even throw together a library to share those magical beans.  However, there's an even better way; one which promotes more complete re-use along with semantic configuration markup and even the ability to standardise configuration of default SI components.  

## Intro

### Audience

This article assumes the reader is familiar with both the EAI Patterns [Hophe-Woolf:2003] and SI itself [SI:2012].  If you're not, and you're game for a laugh, then read on.  Otherwise, I encourage you to read the EAI Patterns online as well as the SI documentation before trying out some of the example projects available on github.  Links are provided at the end of this article.

### Why extend?

If custom components can be simply shared as code libraries, why should we consider extending SI?  To answer that question, let's first agree on the goals for re-use.  Well, we're short on time, let me just _tell_ you what the goals are!

 1. DRY (Don't Repeat Yourself) - maximum re-use, minimum repetition
 1. Convention over configuration
 1. Semantic - i.e. meaningful

To stay DRY (Don't Repeat Yourself), we want the users of our custom components to use convention over configuration.  The custom components should use sensible defaults which cover the majority of use cases, but should also be flexible enough to allow overrides.  At the same time, the configuration should hold semantic meaning.  It should be obvious exactly what has been added to a project where it is used.   

### Methods of re-use

So what method should be used to achieve the aforementioned re-use goals? Here are the options that come to mind:

 1. Shared library
 1. Shared library & configuration file
 1. Extension (with namespace support)

A shared library (1) is good start; it's already avoiding duplication of code and it can even allow for convention over configuration.  Wait though, even the simplest configuration will be duplicated accross multiple projects.  Imagine a core configuration change in the future that could end up touching every project... ouch. It's also not semantic enough.  Sure, the beans can have decent names, but that's about how far it goes.

Aha!  So the answer is to share the configuration file (2) in addition to the code!  Projects can then simply import the configuration file, easy!  So, you'll be creating configuration files for every conceivible combination? Oh, good point.  That's not such a good idea.  As for semantic meaning, all you really get is an appopriately named configuration file.

Enter option 3 as a full blown extension with XML namespace support, which isn't as complex as it sounds, as you will soon discover.  Let's tackle comparison with our goals in reverse order... Semantic?  Yes!  Now you have an XML element with attributes following good naming patterns defined in XML Schema, which of course, is documented.   Convention over configuration?  Sure, most - if not all - attributes have default values.  How DRY is it?  Well, the code and configuration has been abstracted away from each project so there's no repetition on that front.  There's also now a clearly defined boundary, an API if you will.  Refactoring will be simple enough.  Wait, there's even versioning now!  You can maintain different schema versions.

### What about those Domain Spe... thingys?

The discerning reader may be questioning why there's no consideration of a domain-specific language (DSL) as part of the solution.  After all, there's DSLs for SI available in Scala and Groovy!  The reason is to keep things simple.  Some environments don't readily lend themselves to new languages.  This was certainly a constraint I experienced with a client recently.  So, we'll stay with the XML configuration which is, at least, an accessible starting point.  

## The art and craft

Allow me to take 

### Scissors

### Colouring-in

### Glue



## Summary

### The present state

The preceding text has described

### The future goals

 * New adapters 
  * ZIP
  * SNMP
 * Scala DSL

## References

 * [Hophe-Woolf:2003]- http://www.eaipatterns.com/
 * [SI:2012] - http://static.springsource.org/spring-integration/reference/htmlsingle/
