#Streaming updates with SPDY and Netty 4
##Far from the usual
Since i am limited to 1500 words, my obvious language of choice will be 
Scala here, so i presume basic english knowledge in order to understand 
concepts like "map", "foreach" as well as "none" and "some". 
This also limits me in terms of what topic to write about and how detailed 
it can get, but lets just get started with some basic things like


#Differences between SPDY and WebSockets
##Yes, they are fundamental
I've read a lot of people asking questions about how to utilize SPDY to make 
their Web-Sites load faster, which is a valid use-case for this protocol, but 
in return is very unappealing to me, since i rarely do frontend work anymore. 
So what else can we use SPDY for? Well, if you dig through the protocol specifications,
you might see similarities to WebSockets, which is really a distant cusin, except for WebSocket's inability to transfer Data as flexible as SPDY. On the other hand, 
WebSockets can be cleanly handled and started from JavaScript, which i did not 
discover for SPDY yet. In order to get more control over SPDY requests, we can 
use the JSONP concept by giving the backend a callback method to call with 
response-data. This makes the whole scenario a bit more fail-safe, as you at 
least can track the response in JavaScript rather than having it sent raw 
JavaScript. We won't need it for our example, but it is trivial to read the 
callback parameter and wrap your resulting JSON object inside the function call. 
A typical JSONP-style response would look like this:

```
Server: my netty server
Content-Type: application/javascript

callbackMethod(< JSON payload generated in backend>)
```

Why would i pass JSON instead of passing massive amounts of JavaScript? 
Well i find it dauning to code-wrap from Scala into JavaScript, unless we're 
using Liftweb's supreme JavaScript library, we should go with JSON objects 
instead of return lots of JavaScript functions. Have your JavaScript callback 
do the appropriate actions upon receiving the JSON object. This solves odd 
backend-frontend-code-relation issues i've seen in other projects. As said, 
within Liftweb it's absolutely easy and typesafe (!!) to generate JavaScript 
which gets sent to the Browser.

To sum up, SPDY is great to improve page-speed on your Websites and it can 
also be used to live-stream data to clients. SPDY is practically a persistent 
tunnel for all your HTTP/AJAX Requests, thus not requiring your JavaScripts 
to be changed to make use of it.

Great, now that the cat is out of the bag, the next question would be


#What is Netty and what can you do with it

Netty is an Asynchronous Event-Driven Network I/O Framework. It has a very 
clean architecture, committed and genious maintainers, easy to use and best 
of all, it scales (which comes in handy after prototyping). The team also just 
released (after almost a year of development) Netty 4.0.0.Final which is a 
freakishly awesome Milestone as far as i'm concerned. There are a lot of 
changes to watch out for when migrating from Netty 3, but in general, mid-sized 
projects should be able to migrate fairly quickly. Since we're using Scala 
there is another win: the new Netty 4 API is so much easier to use from 
Scala than it was before. Not to forget, Netty has built-in capabilities 
for many many protocols. Some of the higher level like HTTP, WebSockets and 
SPDY are just drop-dead easy to implement, quite the same goes for lower-level 
protocols like raw UDP and TCP services.

As you might have already guessed, Netty is my #1 choice for anything network 
related. Therefor i've implemented several RFC compliant servers and clients, 
most notably netflow.io, which has a NetFlow collector written in Scala using 
Netty's ByteBuffers. ByteBufs are a really good way of dealing with Java's 
native Buffers, but since we're going to work with SPDY, Netty's default HTTP 
capabilities will take care of everything for us, so we don't need to worry 
about juggling bits and bytes ourselves.


##So what are we using all this for?

We will be writing a simple time-streaming application which will get its time 
pushed by Netty. All clients will get exact same Time written, as we will utilize 
Netty's ChannelGroups in order to save performance. This thread-safe class allows 
us to group connected channels and write messages to them with minimal resource 
usage. If you were to write the current time to 50.000 connected clients, 
wouldn't you rather allocate 1 String in memory as opposed to allocating it 
50.000 times and having it produce GC pressure like hell? At least i would.

While a WebSockets via SPDY specification is on its way, we will be streaming 
responses to the client in form of

```
Content-Type: application/javascript

$("#time").text("14:14:14");
$("#date").text("07/07/2013");
```

This does not really make for great error handling, but to be honest, if 
someone misses a second, nobody will die (i hope at least).


## The Server

Writing a Server in Netty is often called "a lot of boilderplate work", 
which really is *not* the case in Scala. As you can see in the next example, 
it's straight up Handlers which need to be chained in the right order. Since 
SPDY is built on top of SSL, which in my opinion is a must have for APIs, 
we use a Bogus SSL[Secure Socket Layer] CA[Certification Authority] which is hardcoded.

```scala
val group = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
val srv = new ServerBootstrap
srv.group(eventLoop1.get, eventLoop2.get)
   .localAddress(addr)
   .channel(classOf[NioServerSocketChannel])
   .childOption[java.lang.Boolean](ChannelOption.TCP_NODELAY, true)
   .childOption[java.lang.Boolean](ChannelOption.SO_KEEPALIVE, true)
   .childOption[java.lang.Boolean](ChannelOption.SO_REUSEADDR, true)
   .childOption[java.lang.Integer](ChannelOption.SO_LINGER, 0)
   .childHandler(new ChannelInitializer[SocketChannel] {

     override def initChannel(ch: SocketChannel) {
       val pipeline = ch.pipeline()
       // add SSL
       val engine = context.createSSLEngine()
       engine.setUseClientMode(false)
       pipeline.addLast("ssl", new SslHandler(engine))
       // add the dispatch handler
       pipeline.addLast("chooser", new SpdyOrHttpHandler(group))
     }
   })
  srv.bind().syncUninterruptibly()
```

## The SpdyOrHttpHandler
Since we want to handle HTTP as well as SPDY (also one builds on the other), we 
need some kind of dispatching mechanism, which SpdyOrHttpHandler will take care of. 
It basically dispatches the Request into a HTTP and SPDY request handler, one of 
which will yield a warning that the browser does not support SPDY. The other one 
responds with the included XML, but we will come to that in the next example.

```scala
class SpdyOrHttpHandler(group: ChannelGroup) 
  extends SpdyOrHttpChooser(8192, 8192) {
  override protected def getProtocol(engine: SSLEngine): SelectedProtocol = {
    protocol match {
      case Some("spdy/2") => SelectedProtocol.SPDY_2
      case Some("spdy/3") => SelectedProtocol.SPDY_3
      case Some("http/1.0") => SelectedProtocol.HTTP_1_0
      case Some("http/1.1") => SelectedProtocol.HTTP_1_1
      case _ => SelectedProtocol.UNKNOWN
    }
  }

  val xml = <html>
              <head>
                <title>wasted.io SPDY Time</title>
                <script src="//ajax.googleapis.com/ajax/libs/jquery/2.0.2/jquery.min.js"></script>
              </head>
              <body>
                <div>
                  Current Date is:<p id="date">unavailable</p><br/>
                  Current Time is:<p id="time">unavailable</p><br/>
                </div>
              </body>
            </html>

  override protected def createHttpRequestHandlerForHttp() =
    new HttpRequestHandler(Text("Served via HTTP. This means that your browser does not support SPDY."))

  override protected def createHttpRequestHandlerForSpdy() = {
    new HttpRequestHandler(xml, (ctx) => group.add(ctx.channel()))
  }
}
```

##The HttpRequestHandler
This request handler is used for both, SPDY and HTTP. As you might guess, 
the body-parameter includes the body from the previous code. In the case of a SPDY 
Frame, we passed a function which will add the established SPDY connection onto an 
List (ChannelGroup), which later will be written to.

```scala
class HttpRequestHandler(body: NodeSeq, handle: (ChannelHandlerContext) => Any = (ChannelHandlerContext) => true)
  extends SimpleChannelInboundHandler[HttpRequest]() {
  override def messageReceived(ctx: ChannelHandlerContext, msg: HttpRequest) {
    if (HttpHeaders.is100ContinueExpected(msg)) ctx.write(OurHttpResponse(CONTINUE))

    val keepAlive = HttpHeaders.isKeepAlive(msg)
    val response = OurHttpResponse(OK, Some(body.toString()), Some("text/plain; charset=UTF-8"), keepAlive)
    val future = ctx.write(response)
    if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
    handle(ctx)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
    ctx.close()
  }
}
```

##Other SPDY related projects
Since it did not fit in here, but i felt compelled to show off, i wrote a very 
basic SPDY proxy and put it on Github, you can find it at [github.com/wasted/spdy-proxy].


##Final words
If you are already using WebSockets, keep using them for your bi-directional 
communication as you've made a great choice. If your goal is to stream as much 
data down the pipeline to your clients while having to deal with little JavaScript, 
you should give this a try.
