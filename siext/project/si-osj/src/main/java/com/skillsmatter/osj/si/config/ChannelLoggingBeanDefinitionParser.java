package com.skillsmatter.osj.si.config;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.integration.channel.DirectChannel;
import org.springframework.integration.channel.interceptor.GlobalChannelInterceptorWrapper;
import org.springframework.integration.channel.interceptor.WireTap;
import org.springframework.integration.config.ConsumerEndpointFactoryBean;
import org.springframework.integration.handler.LoggingHandler;
import org.springframework.integration.handler.MessageHandlerChain;
import org.springframework.util.StringUtils;
import org.w3c.dom.Element;

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

    private String createAndRegisterLogChannel(final ParserContext parserContext) {
        BeanDefinitionBuilder logChannelBuilder = BeanDefinitionBuilder.genericBeanDefinition(DirectChannel.class);
        return registerBeanDefinition(parserContext, logChannelBuilder.getBeanDefinition());
    }

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

    /**
     * Convenience method for registering a bean with a name via the parser context.
     * @param parserContext the parser context which is used to register the bean
     * @param beanDef the bean definition itself
     * @return the generated name assigned to the bean
     */
    private String registerBeanDefinition(final ParserContext parserContext, final AbstractBeanDefinition beanDef) {
        return BeanDefinitionReaderUtils.registerWithGeneratedName(beanDef, parserContext.getRegistry());
    }

}
