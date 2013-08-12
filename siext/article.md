Extending Spring Integration
----------------------------

## Abstract

Spring Integration (SI) offers numerous adapters out of the box covering the most common integration needs from web services to messaging systems with databases aplenty inbetween. What happens though when a requirement falls outside of the default offering?  Beans I hear you say?!  True enough, one can easily write new components to handle such scenarios. Say, you could even throw together a library to share those magical beans.  However, there's an even better way; one which promotes more complete re-use along with semantic configuration markup and even the ability to standardise configuration of default SI components.  

## Intro

### Audience

This article assumes the reader is familiar with both the EAI Patterns and SI itself.  If you're not, and you're game for a laugh, then read on.  Otherwise, I encourage you to read the EAI Patterns online as well as the SI documentation before trying out some of the example projects available on github.  Links are provided at the end of this article.

### Why extend?



If custom components can be simply shared as code libraries, why should we consider extending SI?  To answer that question, let's first agree on the goals for re-use.  Well, we're short on time, let me just _tell_ you what the goals are!

 1. DRY (Don't Repeat Yourself)
 1. Convention over configuration
 1. Semantic - i.e. meaningful

To stay DRY, we want the users of our custom components to 

### Methods of re-use

 1. Shared library
 1. Shared library & configuration
 1. Extension (with namespace support)


### What about those Domain Spe... thingys?

The discerning reader may be questioning why there's no consideration of a domain-specific language (DSL) as part of the solution.  After all, there's DSLs for SI available in Scala and Groovy!  The reason is to keep things simple.  Some environments don't readily lend themselves to new languages.  This was certainly a constraint I experienced recently.  So, we'll stay with the XML configuration which is, at least, an accessible starting point.  

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

 * [Kingsbury:2013] - http://aphyr.com/posts/288-the-network-is-reliable
 * [McCallister:2009] - http://skife.org/architecture/fault-tolerance/2009/12/31/bulkheads.html
 * [Wikipedia:CF] - http://en.wikipedia.org/wiki/Cascading_failure
 * [Wikipedia:LT] - http://en.wikipedia.org/wiki/Location_transparency
