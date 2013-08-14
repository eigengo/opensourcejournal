package com.skillsmatter.osj.si.config;

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
