package org.eigengo.osj.si.config;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.integration.Message;
import org.springframework.integration.MessageChannel;
import org.springframework.integration.channel.DirectChannel;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import javax.annotation.Resource;
import java.util.UUID;

import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:log-complete-test.xml")
public class ChannelLoggingBeanDefinitionParserCompleteTest {

    @Autowired
    private ApplicationContext ctx;
    @Resource(name = "testRequest")
    private MessageChannel inChannel;
    @Autowired
    private InMemoryMessageProcessorService service;

    @Test
    public void shouldLoadDefaultAuditConfiguration() {
        assertNotNull(ctx.getBean("org.springframework.integration.channel.DirectChannel#0", DirectChannel.class));
        assertEquals(2, ctx.getBeansOfType(GlobalChannelInterceptorBeanPostProcessor.class).size());
    }

    @Test
    public void configuredBeansShouldHandleMessage() throws Exception {
        final String TEST_TRACKING_ID_HEADER = "TEST_TRACKING_ID";
        Message<String> testMsg = MessageBuilder.withPayload("This is a test message")
                .setHeader(TEST_TRACKING_ID_HEADER, UUID.randomUUID()).build();
        assertTrue(inChannel.send(testMsg));
        Message<String> matchingMsg = null;
        for (Message<String> msg : service.getProcessedMessages()) {
            if (msg.getHeaders().get(TEST_TRACKING_ID_HEADER).equals(testMsg.getHeaders().get(TEST_TRACKING_ID_HEADER)))
                matchingMsg = msg;
        }
        assertNotNull(matchingMsg);
    }


}
