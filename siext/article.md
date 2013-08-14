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
 1. Shared library & configuration files
 1. Extension (with namespace support)

A shared library (1) is good start; it's already avoiding duplication of code and it can even allow for convention over configuration.  Wait though, even the simplest configuration will be duplicated accross multiple projects.  Imagine a core configuration change in the future that could end up touching every project... ouch. It's also not semantic enough.  Sure, the beans can have decent names, but that's about how far it goes.

Aha!  So the answer is to share the configuration file (2) in addition to the code!  Projects can then simply import the configuration file, easy!  So, you'll be creating configuration files for every conceivible combination? Oh, good point.  That's not such a good idea.  As for semantic meaning, all you really get is an appopriately named configuration file.

Enter option 3 as a full blown extension with XML namespace support, which isn't as complex as it sounds - as you will soon discover.  Let's tackle comparison with our goals in reverse order... Semantic?  Yes!  Now you have an XML element with attributes following good naming patterns defined in XML Schema, which of course, is documented.   Convention over configuration?  Sure, most - if not all - attributes have default values.  How DRY is it?  Well, the code and configuration has been abstracted away from each project so there's no repetition on that front.  There's also now a clearly defined boundary, an API if you will.  Refactoring will be simple enough.  Wait, there's even versioning now!  You can maintain different schema versions.

### What about those domain-specific thingys?

The discerning reader may be questioning why there's no consideration of a domain-specific language (DSL) as part of the solution.  After all, there's DSLs for SI available in Scala and Groovy!  The reason is to keep things simple.  Some environments don't readily lend themselves to new languages.  This was certainly a constraint I experienced with a client recently.  So, we'll stay with the XML configuration which is, at least, an accessible starting point.  

## The art and craft of an extension

Right, now it's time to start looking at code.  What is our custom component going to be?  Unfortunately there's not the time nor space to talk you through a complete message auditing scenario, one which I implemented for a client recently. Instead, for brevity, I'll show you a fairly simple component which offers logging of messages across multiple channels at a certain log level.  This will be achived using a `WireTap` to send messages to a logging `DirectChannel`.  Messages copied across to this channel will be handled with the SI component `LoggingHandler`.  This handler could easily be some other SI component or even our own custom bean.     

Writing an extension to Spring Integration really isn't that difficult.  I suppose you could compare it with colouring, cutting and sticking.  In fact, let's stay with that analogy.

### Scissors

First of all we need to cut the shape of component.  Our logging component will be used in the following ways:

######Listing 1. Example usage of the log component

```xml
<?xml version='1.0' encoding='UTF-8'?>
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:int="http://www.springframework.org/schema/integration"
       xmlns:int-osj="http://skillsmatter.com/osj/schema/integration"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
        http://www.springframework.org/schema/integration http://www.springframework.org/schema/integration/spring-integration.xsd
        http://skillsmatter.com/osj/schema/integration http://skillsmatter.com/osj/schema/integration/spring-integration-osj-1.0.xsd">
  
  <int-osj:log />
  <int-osj:log channel-pattern="*.in"  />
  <int-osj:log channel-pattern="*.out" level="INFO" />

  ...

</beans>
```

Notice the `int-osj` namespace?  That's our extension point for configuration.  Next up, we need a schema like so:

######Listing 2. Schema for the OSJ namespace

```xml
<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns="http://skillsmatter.com/osj/schema/integration"
            xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            targetNamespace="http://skillsmatter.com/osj/schema/integration"
            elementFormDefault="qualified" attributeFormDefault="unqualified">

    <xsd:annotation>
        <xsd:documentation>
                Defines the configuration elements for the Spring Integration OSJ Adapter.
        </xsd:documentation>
    </xsd:annotation>

    <xsd:element name="log">
        <xsd:annotation>
            <xsd:documentation>
                Enables logging of messages as they move between intercepted channels.

                The channels to be intercepted are defined by the channel-pattern attribute.

                The level at which the messages are logged is defined by the level attribute

                A reasonable default value is provided for each attribute which should cover most integration scenarios.
            </xsd:documentation>
        </xsd:annotation>
        <xsd:complexType>
            <xsd:attribute name="channel-pattern" type="xsd:string" use="optional" default="*.in, *.out, *.error">
                <xsd:annotation>
                    <xsd:documentation>
                        Channel name(s) or patterns. To specify more than one channel use
                        ',' 
                        (e.g.,
                        channel-name-pattern="input*, foo, bar")
                        Default value is *.in, *.out, *.error
                    </xsd:documentation>
                </xsd:annotation>
            </xsd:attribute>
            <xsd:attribute name="level" type="xsd:string" use="optional" default="DEBUG">
                <xsd:annotation>
                    <xsd:documentation>
                        Level at which message should be logged, one of: FATAL, ERROR, WARN, INFO, DEBUG, TRACE
                        Default value is DEBUG.
                    </xsd:documentation>
                </xsd:annotation>
            </xsd:attribute>
        </xsd:complexType>
    </xsd:element>

</xsd:schema>
```

Excellent, we have now cut the definition of our custom component.  Notice the default values providing convention over configuration, while the element and attribute names along with their respective documentation satisfy our semantic configuration goal.


### Colouring-in

Now to add the colour; the functionality.  Spring XML config is no use without a parser to register the appropriate beans in the Spring context.  As you might imagine, Spring have a defined interface for parsers: `BeanDefinitionParser`.  The entry point however, lies with the interface `NamespaceHandler`.  Spring Integration offer the `AbstractIntegrationNamespaceHandler` which cuts out some of the boilerplate.  The responsibility of the `NamespaceHandler` is to register beans for XML elements in a certain namespace.  It may register beans directly, but most often registers parsers which essentially allows it to delegate the bean registration work.  This aids modularity so we'll stick with that approach

######Listing 3. The OSJ NamespaceHandler

```java
import org.springframework.integration.config.xml.AbstractIntegrationNamespaceHandler;

/**
 * Handles parsing of int-osj{http://skillsmatter.com/osj/schema/integration}
 * namespace configuration elements.
 */
public class OsjNamespaceHandler extends AbstractIntegrationNamespaceHandler {

    /**
     * Initializer which registers bean definition parsers for elements in the namespace
     */
    @Override
    public void init() {
        registerBeanDefinitionParser("log", new ChannelLoggingBeanDefinitionParser());
    }
}
```

So now onto the class where the real work is done:

######Listing 4. The log XML element parser

```java
/**
 * Parser for the <code><log/></code> element in the
 * int-osj{http://skillsmatter.com/osj/schema/integration} namespace
 */
public class ChannelLoggingBeanDefinitionParser implements BeanDefinitionParser {

    /**
     * Parses a log element into multiple bean definitions required for logging of messages.
     * @param element the audited XML element
     * @param parserContext provided by Spring
     *                      and offers access to {@link org.springframework.beans.factory.support.BeanDefinitionRegistry}
     * @return <code>null</code> because this parser registers multiple beans using the {@link ParserContext}
     * rather than a single bean.
     */
    @Override
    public BeanDefinition parse(final Element element, final ParserContext parserContext) {
        final String logChannelName = createAndRegisterLogChannel(parserContext);
        createAndRegisterLogWireTap(element, parserContext, logChannelName);
        createAndRegisterLogMessageHandlerChain(element, parserContext, logChannelName);
        return null;
    }

 	// private methods

 }
```

You can see the entry point to the parser is the overriden method `parse` which receives the XML element being processed by Spring as well as the parsing context.  You may notice the method returns `null`.  I'm certainly no advocate of returning `null` but unfortunately, the `BeanDefinitionParser` forces us down the route of returning _something_.  We'll look at each of the three private methods which I call out to but first let me show you the convenience method used to register beans via the `parserContext`:

######Listing 5. Convenience method of parser

```java
    /**
     * Convenience method for registering a bean with a name via the parser context.
     * @param parserContext the parser context which is used to register the bean
     * @param beanDef the bean definition itself
     * @return the generated name assigned to the bean
     */
    private String registerBeanDefinition(final ParserContext parserContext, final AbstractBeanDefinition beanDef) {
        return BeanDefinitionReaderUtils.registerWithGeneratedName(beanDef, parserContext.getRegistry());
    }
```

OK, let's take each of the three private methods in turn, starting with `createAndRegisterLogChannel()`.  This method creates a bean definition programmatically via a `DirectChannel` which is the default channel implementation in SI.  This is the channel to which we'll be able to send wire tapped messages.  The method's definition is simply:

######Listing 6. Log channel registration in parser

```java
    private String createAndRegisterLogChannel(final ParserContext parserContext) {
        BeanDefinitionBuilder logChannelBuilder = BeanDefinitionBuilder.genericBeanDefinition(DirectChannel.class);
        return registerBeanDefinition(parserContext, logChannelBuilder.getBeanDefinition());
    }
```

Notice we return a `String` which is the generated bean name for the log channel.  This comes in handy when we call the next two private methods:

######Listing 7. Log channel handler chain registration in parser

```java
    private void createAndRegisterLogMessageHandlerChain(final Element element, final ParserContext parserContext, final String logChannelName) {
        BeanDefinitionBuilder loggingHandlerBuilder = BeanDefinitionBuilder.genericBeanDefinition(LoggingHandler.class);
        loggingHandlerBuilder.addConstructorArgValue(element.getAttribute("level"));
        loggingHandlerBuilder.addPropertyValue("shouldLogFullMessage", true);
        final String loggingHandlerBeanName = registerBeanDefinition(parserContext, loggingHandlerBuilder.getBeanDefinition());

        ManagedList<RuntimeBeanReference> handlerRefList = new ManagedList<RuntimeBeanReference>();
        handlerRefList.add(new RuntimeBeanReference(loggingHandlerBeanName));

        BeanDefinitionBuilder chainBuilder = BeanDefinitionBuilder.genericBeanDefinition(MessageHandlerChain.class);
        chainBuilder.addPropertyValue("handlers", handlerRefList);
        final String logMessageHandlerChainBeanName = registerBeanDefinition(parserContext, chainBuilder.getBeanDefinition());

        BeanDefinitionBuilder consumerEndpointbuilder = BeanDefinitionBuilder.genericBeanDefinition(ConsumerEndpointFactoryBean.class);
        consumerEndpointbuilder.addPropertyReference("handler", logMessageHandlerChainBeanName);
        consumerEndpointbuilder.addPropertyValue("inputChannelName", logChannelName);
        BeanDefinitionReaderUtils.registerWithGeneratedName(consumerEndpointbuilder.getBeanDefinition(), parserContext.getRegistry());
    }
```

There's a little bit more going on here.  We're setting up the handler chain for the log channel.  This is where we configure the `LoggingHandler`.  Notice that we make use of the "level" attribute from our `<log/>` element here to set the logging level.  There's a little bit more lifting to do with regards to dealing with `BeanReference`'s.  One has to remember that Spring will peform post processing of the bean definitions to arrive at the final application context. If you're not familiar with this concept, I'll leave you with that as some homework.  Finally in this method, we need to tie the handler chain to the message channel.  That's achieved using a `ConsumerEndpointFactoryBean`.

Finally, let's look at the private method to setup the wire tapping:

######Listing 8. Log wire tap registration in parser

```java
    private void createAndRegisterLogWireTap(final Element element, final ParserContext parserContext, final String logChannelName) {
        BeanDefinitionBuilder logWireTapBuilder = BeanDefinitionBuilder.genericBeanDefinition(WireTap.class);
        logWireTapBuilder.addConstructorArgReference(logChannelName);
        final String logWireTapBeanName = registerBeanDefinition(parserContext, logWireTapBuilder.getBeanDefinition());

        BeanDefinitionBuilder logWireTapWrapperBuilder = BeanDefinitionBuilder.genericBeanDefinition(GlobalChannelInterceptorWrapper.class);
        logWireTapWrapperBuilder.addConstructorArgReference(logWireTapBeanName);
        String patternAttribute = element.getAttribute("channel-pattern");
        String[] patterns = StringUtils.trimAllWhitespace(patternAttribute).split(",");
        logWireTapWrapperBuilder.addPropertyValue("patterns", patterns);
        final String logWireTapWrapperBeanName = registerBeanDefinition(parserContext, logWireTapWrapperBuilder.getBeanDefinition());

        BeanDefinitionBuilder postProcessorBuilder = BeanDefinitionBuilder.genericBeanDefinition(GlobalChannelInterceptorBeanPostProcessor.class);
        ManagedList<RuntimeBeanReference> globalInterceptors = new ManagedList<RuntimeBeanReference>();
        globalInterceptors.add(new RuntimeBeanReference(logWireTapWrapperBeanName));
        postProcessorBuilder.addConstructorArgValue(globalInterceptors);
        registerBeanDefinition(parserContext, postProcessorBuilder.getBeanDefinition());
    }
```

A `WireTap` definition is created with a reference to the log channel using the provided `logChannelName` parameter.  Then we make use of `GlobalChannelInterceptorWrapper` which allows us to define channels we would like to intercept.  Notice we make use of the `channel-pattern` element from our `<log/>` element here.  Finally, we use a `GlobalChannelInterceptorBeanPostProcessor` to marry intercepted channels with global interceptors.  Our global interceptor here is the `WireTap` class.

### Glue

Finally, to make our component an extension we need some glue.  That comes in the form of a resource bundle under the `META-INF` directory on the classpath which Spring will search for when parsing a new context.  This files that form the resource bundle are `spring.schemas`, `spring.handlers` and optionally `spring.tooling`.  These simple property files for our use case look like so:

######Listing 9. spring.schemas file

```
http\://skillsmatter.com/osj/schema/integration/spring-integration-osj-1.0.xsd=com/skillsmatter/osj/si/config/spring-integration-osj-1.0.xsd
http\://skillsmatter.com/osj/schema/integration/spring-integration-osj.xsd=com/skillsmatter/osj/si/config/spring-integration-osj-1.0.xsd
```

This file maps the schema reference used in Spring context files (think schemaLocation) to the actual location on the classpath.  We provide mappings for references with and without the version qualifier.

######Listing 10. spring.handlers file

```
http\://skillsmatter.com/osj/schema/integration=com.skillsmatter.osj.si.config.OsjNamespaceHandler
```

This file maps the namespace in which our components live to the class which can ultimately handle the parsing of the XML elements.

######Listing 11. spring.tooling file

```
# Tooling related information for the OSJ Spring Integration namespace
http\://skillsmatter.com/osj/schema/integration@name=OSJ Spring Integration namespace
http\://skillsmatter.com/osj/schema/integration@prefix=int-osj
http\://skillsmatter.com/osj/schema/integration@icon=com/skillsmatter/osj/si/config/spring-integration-skillsmatter.gif
```

This optional file is used by tools (such as IDEs) to assign a friendly name and icon to the namespace.  Additionally, a prefix is defined as a suggested name for use in target XML files.

So there we have it!  Head over to https://github.com/eigengo/opensourcejournal/blob/2013.1/siext/project to see the whole project in context.

## Summary

The preceding text has described how to create a basic extension for Spring Integration.  We've discovered that an extension offers maximum re-use over alternative approaches which include shared code and resources as well as meaningful configuration.  I hope you'll be encouraged to try bundling your favoured patterns / components into an extension in the near future.  Always consider submitting your extension back to Spring via a github pull request.

Talking about the future, I think Spring Integration could do with adapters for ZIP handling and SNMP...

## References

 * [Hophe-Woolf:2003]- http://www.eaipatterns.com/
 * [SI:2012] - http://static.springsource.org/spring-integration/reference/htmlsingle/

