# Pushing data in Scala through SPDY with Netty

# What's this about?

If you follow IT news and did not stumble upon my article by chance, then you probably already know about Google's SPDY Protocol. If you haven't heard about it, SPDY (which is pronounced speedy) is somewhat of a transport-layer. It builds on SSL and if you can imagine a typical HTTP request cycle, SPDY will act as kind of "tunnel" between the browser and a SPDY-enabled WebServer which will stay connected far longer than your request might take to complete.

Once the Browser starts tunneling (or aggregating) the HTTP request->response cycle into a **single already established connection**, the latency will be lower and thus request times are faster, as the client won't make separate connections for each CSS, JavaScript or Image-resource it requests from the Server. This is especially good for mobile clients which usually have huge latency compared to typical DSL lines at residential homes. Recent tests have shown, that SPDY is a not really that much faster, but reduces connection overhead, which comes in handy because SSL handshakes between servers and clients are expensive (for the server mostly).

Another feature of SPDY is "push". The server can push data to the client before it gets "consumed". I choose this word wisely because I already tripped up in implementation errors on an earlier draft of this article. The issue being that while WebSocket Frames trigger a handler on the Client, pushed SPDY frames do not. The only way to utilize pushed SPDY resources is to "use" them inside the Browser's context, for example ```<img src=../>``` or ```<script href=../>```. So "consume" feels like the right word here.

# What's going to happen?

We will be writing a simple SPDY-capable webserver and push some CSS and JavaScript before the browser consumes it. As the title suggests, we will be using Scala throughout the Article, as i love it for the Syntax and Typesafety. 


# Differences between SPDY and WebSockets

Digging through the protocol specifications[1,2,3], one might see similarities to WebSockets, which is really a distant cousin, except for WebSocket's inability to transfer data as flexible as SPDY. On the other hand, WebSockets can be cleanly handled and started from JavaScript, which was not a design goal in SPDY. So it is virtually impossible to get a handle on a resource pushed via SPDY unless the browser "consumes" those pushed resources.

To make it absolutely clear what I mean, WebSockets have an onMessage (or something similar) callback which gets triggered on every WebSocket received from a particular host. In SPDY there is **nothing** like it!

To sum up, SPDY is practically a persistent tunnel for all HTTP/AJAX Requests, thus not requiring JavaScripts to be changed to make use of it. It is **not** a WebSocket replacement!

Great, now that the cat is out of the bag, the next question would be

# What is Netty and what can we do with it

Netty is an Asynchronous Event-Driven Network I/O Framework. It has a very clean architecture, committed and genious maintainers, easy to use and best of all, it scales (which comes in handy after prototyping). The team also just released (after almost a year of development) Netty 4.0.0.Final which is a freakishly awesome Milestone as far as i'm concerned. There are a lot of changes to watch out for when migrating from Netty 3, but in general, mid-sized projects should be able to migrate fairly quickly. Since we're using Scala there is another win: the new Netty 4 API is so much easier to use from Scala than it was before. Not to forget, Netty has built-in capabilities for many many protocols. Some of the higher level like HTTP, WebSockets and SPDY are just drop-dead easy to implement, quite the same goes for lower-level protocols like raw UDP and TCP services.

As you might have already guessed, Netty is my #1 choice for anything network related. Therefor i've implemented several RFC compliant servers and clients, most notably netflow.io, which has a NetFlow collector written in Scala using Netty's ByteBuffers. ByteBufs are a really good way of dealing with Java's native NIO Buffers, but since we're going to work with SPDY, Netty's default HTTP capabilities will take care of everything for us, so we don't need to worry about juggling bits and bytes ourselves.


# The Server

Writing a server in Netty is often called "a lot of boilderplate work", which really is *not* the case in Scala. The first example (Listing 1) shows, it's straight up handlers which need to be chained in the right order. Since SPDY is built on top of SSL, which in my opinion is a must have for APIs, we use a bogus SSL[Secure Socket Layer] CA[Certification Authority] which is hardcoded.

###### Listing 1. The Server

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

# The Next Protocol Negotiation Provider

In order for clients to negotiate SPDY, the server needs a piece of code called the "Next Protocol Negotiation Provider", short NPN Provider. This allows us to do SPDY-version handling vs. normal HTTP requests.

We simply create a class of our own extending Jetty's (**not Netty's**) NPN Provider Interface. We need to provide all supported protocol versions (in our example being SPDY2 and 3 as well as HTTP/1.1). Since we are going to be used by Java, we need some Java-style setters and getters.


###### Listing 2. The NPN Provider

´´´scala
    class NpnServerProvider extends ServerProvider {
      private final val default: String = "http/1.1"
      private final val supported = List("spdy/2", "spdy/3", "http/1.1").asJava

      private var protocol: String = _
      def getSelectedProtocol = protocol

      override def protocolSelected(proto: String) {
        protocol = proto
      }

      override def unsupported() {
        protocol = default
      }

      override def protocols() = supported
    }
```



# The SpdyOrHttpHandler

We also want to handle SPDY as well as HTTP (one builds on the other), we need some kind of dispatching mechanism, which SpdyOrHttpHandler will take care of. It basically dispatches the request into a HTTP and SPDY request handler, one of which will yield a warning that the browser does not support SPDY. The other one responds with the included XML, but we will come to that in the next example.

###### Listing 3. The SpdyOrHttpHandler

```scala
class SpdyOrHttpHandler extends SpdyOrHttpChooser(1024 * 1024, 1024 * 1024) {
  override protected def getProtocol(engine: SSLEngine): SelectedProtocol = {
    val provider = NextProtoNego.get(engine).asInstanceOf[NpnServerProvider]
    val protocol = Option[String](provider.getSelectedProtocol)
    println(s"NPN Provider: ${provider.toString}: $protocol")
    protocol match {
      case Some("spdy/2") => SelectedProtocol.SPDY_2
      case Some("spdy/3") => SelectedProtocol.SPDY_3
      case Some("http/1.0") => SelectedProtocol.HTTP_1_0
      case Some("http/1.1") => SelectedProtocol.HTTP_1_1
      case _ => SelectedProtocol.UNKNOWN
    }
  }

  override protected def createHttpRequestHandlerForHttp() = new HttpRequestHandler
  override protected def createHttpRequestHandlerForSpdy() = new SpdyRequestHandler
}
```

# Our HTTP Helper

In order to craft valid HTTP responses, I am using a helper to make it short and sweet. This particular helper saved me countless lines. Since lots of the stuff is Option'al, it's pretty flexible to use. I use this helper almost on a daily basis.

###### Listing 4. Our HTTP Helper

```scala
object OurHttpResponse {
  lazy val serverToken = Some("wasted.io spdy")

  def apply(
    version: HttpVersion,
    status: HttpResponseStatus,
    body: Option[String] = None,
    mime: Option[String] = None,
    close: Boolean = true,
    headers: Map[String, String] = Map()): FullHttpResponse = {
    val res = body match {
      case Some(body) =>
        val content = Unpooled.wrappedBuffer(body.getBytes("UTF-8"))
        val res = new DefaultFullHttpResponse(version, status, content)
        setContentLength(res, content.readableBytes())
        res
      case None =>
        val res = new DefaultFullHttpResponse(HTTP_1_1, status)
        setContentLength(res, 0)
        res
    }

    mime match {
      case Some(contenttype) => res.headers.set(CONTENT_TYPE, contenttype)
      case _ =>
    }

    serverToken.foreach { t => res.headers.set(SERVER, t) }
    headers.foreach { h => res.headers.set(h._1, h._2) }

    if (close) res.headers.set(CONNECTION, Values.CLOSE)
    res
  }
}
```

# The HttpRequestHandler

This request handler is used for both, SPDY and HTTP. The body-parameter includes the body from the previous code.

###### Listing 5. The HttpRequestHandler

```scala
class HttpRequestHandler extends SimpleChannelInboundHandler[FullHttpRequest]() with Logger {

  override def channelRead0(ctx: ChannelHandlerContext, msg: FullHttpRequest) {
    val keepAlive = HttpHeaders.isKeepAlive(msg)
    val body = "Served via HTTP. This means that your browser does not support SPDY."
    val contenType = "text/html; charset=UTF-8"
    val response = OurHttpResponse(msg.getProtocolVersion, OK, Some(body), Some(contenType), keepAlive)
    val future = ctx.channel().writeAndFlush(response)
    if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
    ctx.close()
  }
}
```

# The SpdyRequestHandler

Now here comes the real sugar, the pushContent method is specifically designed to pre-push resources on another SPDY stream back to the browser, referencing the initial (associated) stream id. If we were to serve HTTP pages from files, we could parse them for "href" or "src" attributes and pre-push all those resources. In our example, we just push a simple "site.js" which does nothing more than an alert, and we also push a style.css which will just make up for a gray background. Since we're 404'ing everything but /, we can be sure that these resources will not be pulled with a traditional GET request.

###### Listing 6. The SpdyRequestHandler

```scala
class SpdyRequestHandler extends HttpRequestHandler {
  final val body = {
    <html>
      <head>
        <title>SPDY Demo</title>
        <script src="/site.js" type="text/javascript"></script>
        <link href="/style.css" rel="stylesheet"/>
      </head>
      <body>
        SPDY works!
      </body>
    </html>
  }

  /**
   * SPDY Channel ID the Server uses to push to the client (positive even integer, 2 in our example)
   */
  var ourSpdyId = 2

  /**
   * This is an example for a more granular approach for pushing data
   * @param ctx Netty ChannelHandelerContext
   * @param msg The initial HTTP request
   * @param currentStreamId SPDY Channel ID the Client used to initiate the SPDY request (positive uneven integer)
   * @param name Resource name (e.g. foo.xml)
   * @param message Body to be delivered
   * @param contentType Content-Type to be transmitted in Headers
   * @param fin Finish this stream
   */
  def pushContent(ctx: ChannelHandlerContext, msg: FullHttpRequest, currentStreamId: Int,
                  name: String, message: String, contentType: String, fin: Boolean) {
    // this is not really safe and should not be used in production ;)
    val url = s"https://${msg.headers().get("host")}/$name"
    println(s"Pushing resource: $url")

    // Allocate the buffer for our message
    val buf = Unpooled.copiedBuffer(message, CharsetUtil.UTF_8)
    // Create a SPDY Data frame
    val content = new DefaultSpdyDataFrame(currentStreamId, buf)

    content.setLast(fin)

    // Create headers for the Stream-Id the Server is using and reference the stream-ID the client uses.
    //val headers = new DefaultSpdyHeadersFrame(ourSpdyId)
    val headers = new DefaultSpdySynStreamFrame(ourSpdyId, currentStreamId, 0)
    headers.setUnidirectional(true)
    SpdyHeaders.setStatus(2, headers, OK)
    SpdyHeaders.setVersion(2, headers, msg.getProtocolVersion)
    SpdyHeaders.setUrl(2, headers, url)
    headers.headers().add("Content-Type", contentType)
    headers.headers().add("Content-Length", buf.readableBytes())
    headers.setLast(false)

    ctx.channel().write(headers)
    ctx.channel().write(content)
    ctx.channel().flush()
    println(headers.toString)
    println(content.toString)
    println("----")
  }

  override def channelRead0(ctx: ChannelHandlerContext, msg: FullHttpRequest) {
    val currentStreamId = Option(SpdyHttpHeaders.getStreamId(msg)) getOrElse 0

    // Deny everything gracefully in order to not close the SPDY connect
    // Normally we should deliver those resources by HTTP standards when the client requests them
    // This is the real proof that it does not request the resources by traditional http means
    // (or at least fails it it tries to)
    if (!msg.getUri.matches("/")) {
      println(s"404'ing a request to ${msg.getUri}")
      val headers = new DefaultSpdySynStreamFrame(currentStreamId, 0, 0)
      SpdyHeaders.setStatus(2, headers, NOT_FOUND)
      SpdyHeaders.setVersion(2, headers, msg.getProtocolVersion)
      SpdyHeaders.setUrl(2, headers, msg.getUri)
      ctx.channel().writeAndFlush(headers)
      // we could also do
      //ctx.channel().writeAndFlush(OurHttpResponse(msg.getProtocolVersion, NOT_FOUND, close = false))
      return
    }

    println(msg)
    println("---")

    // Sending a 100 continue if needed
    if (HttpHeaders.is100ContinueExpected(msg)) {
      ctx.channel().writeAndFlush(OurHttpResponse(msg.getProtocolVersion, CONTINUE, close = false))
    }

    // Set some SPDY settings. 4 is the number of maximum streams through this SPDY connection.
    // Other settings would be up- or downstream in kbits (which can be nice since we're going to push resources)
    val settings = new DefaultSpdySettingsFrame
    settings.setValue(4, 300)
    ctx.channel().writeAndFlush(settings)

    // Push our site.js and style.css
    pushContent(ctx, msg, currentStreamId, "site.js", "alert('via SPDY!');", "text/javascript; charset=utf-8", false)
    pushContent(ctx, msg, currentStreamId, "style.css", "body { background-color: gray; }", "text/css", true)

    // Reply on the request (client initiated) stream with the HTML
    // which tells the client to consume pushed resources
    val headers = Map(
      SpdyHttpHeaders.Names.STREAM_ID -> currentStreamId.toString,
      SpdyHttpHeaders.Names.PRIORITY -> "0")

    val contenType = "text/html; charset=UTF-8"
    val response = OurHttpResponse(msg.getProtocolVersion, OK, Some(body.toString()), Some(contenType), false, headers)

    println(response)
    println("---")

    // Write the response to the client
    ctx.channel().writeAndFlush(response)

    // Add a Close-Listener to print a line once the SPDY connection closes
    ctx.channel.closeFuture().addListener(new ChannelFutureListener() {
      override def operationComplete(cf: ChannelFuture) {
        println("SPDY connection closed")
      }
    })
  }
}

```

# Final words

If you are already using WebSockets for your bi-directional communication, keep them as you've made a great choice. If your goal is to stream as much data down the pipeline to your clients while having to deal with little JavaScript, you should give this a try. Don't forget, you can still do AJAX-style long-polling over SPDY.

Since I did not want to put the bogus SSL store in this article (becaus nobody wants to type a full-page Byte-Array), i've put the project on GitHub. The source-code can be found at https://github.com/fbettag/osj-2013-scala-netty4-spdy.


# References

[#1 - RFC6455 WebSockets](http://tools.ietf.org/html/rfc6455)

[#2 - SPDY2](http://www.chromium.org/spdy/spdy-protocol/spdy-protocol-draft2)

[#3 - SPDY3](http://www.chromium.org/spdy/spdy-protocol/spdy-protocol-draft3)

