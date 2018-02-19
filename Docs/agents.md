[<< Previous](./async.md) | [Index](./index.md) | [Next >>](./modules.md)

# Agents and Messages (Actor Model)

Agents allow lightweight concurrency, by receiving and processing messages asynchronously. (See https://en.wikipedia.org/wiki/Actor_model)  

Every `agent` has its own *mailbox*, listening to user-defined messages.  
Agents are always tables:

```corescript
let actor = agent {
    receive say: fun(what) {
        print("Agent sais: ", what);
    }
}
send actor say ("something funny"); // returns unit
// prints: Agent sais: something funnny
```

Messages consist of a *name* and call-arguments.  
Similar to calling an [asynchronous function](./async.md), the `send`-statement will return a `unit`-value.  
The message-processing is done in background.  

Despite function-calls, a *name* not defined as an agents-message-receiver, will not lead to an error.  
Instead, the message will be ignored.  
This allowes very good decoupling, as the sender doesn't need to know anything about the internal behaviors of an agent.  

You can have hundrets or thousands of agents, running simultaneously, without sacrificing performance.