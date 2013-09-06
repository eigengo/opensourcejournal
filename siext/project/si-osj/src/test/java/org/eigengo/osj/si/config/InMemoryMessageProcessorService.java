package org.eigengo.osj.si.config;

import org.springframework.integration.Message;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class InMemoryMessageProcessorService {

    private List<Message> processedMessages = new ArrayList<Message>();


    @ServiceActivator
    public void handleMessage(Message<String> msg) {
        processedMessages.add(msg);
    }

    public List<Message> getProcessedMessages() {
        return processedMessages;
    }
}
